(in-package :info.read-eval-print.httpd)

#|
accept
read request
app
  response stream
    epoll_wait IN
  app queue
  thread pool
|#

(defgeneric start-response (stream))

(defgeneric %write-to-fd (stream))

(defclass response-stream (trivial-gray-streams:trivial-gray-stream-mixin
                           trivial-gray-streams:fundamental-binary-output-stream
                           trivial-gray-streams:fundamental-character-output-stream)
  ((fd :initarg :fd)
   (protocol :initarg :protocol :initform "HTTP/1.1")
   (external-format :initarg :external-format :initform :utf-8
                    :accessor external-format-of)
   (response-status :initarg :response-status :initform 200
                    :accessor response-status-of)
   (response-headers :initarg :respone-headens
                     :initform (list (cons "Date" (rfc-2822-now))
                                     (cons "Content-Type" "text/html; charset=UTF-8"))
                     :accessor response-headers-of)
   (started :initform nil :accessor started-p)
   (request :initarg :request)))

(defclass ssl-response-mixin ()
  ((ssl-stream :initarg :ssl-stream)))

(defclass content-length-response-stream (response-stream)
  ((buffer :initform (make-queue))))

(defclass ssl-content-length-response-stream (ssl-response-mixin content-length-response-stream)
  ())

(defclass chunked-response-stream (response-stream)
  ())

(defclass ssl-chunked-response-stream (ssl-response-mixin chunked-response-stream)
  ())


(defgeneric write-to-response (response buffer length))

(defmethod write-to-response ((response response-stream) (buffer string) length)
  (declare (ignore length))
  (with-slots (fd) response
   (cffi:with-foreign-string ((ptr size) buffer :null-terminated-p nil)
     (sys-write fd ptr size))))

(defmethod write-to-response ((response response-stream) buffer length)
  (with-slots (fd) response
    (typecase buffer
      (octet-vector
       (cffi-sys:with-pointer-to-vector-data (ptr buffer)
         (sys-write fd ptr length)))
      (t                                ;buffer must be a system area pointer.
       (sys-write fd buffer length)))))

(defmethod write-to-response ((response ssl-response-mixin) buffer length)
  (with-slots (ssl-stream) response
    (write-sequence buffer ssl-stream :end length)))


(defmethod initialize-instance :after ((stream chunked-response-stream) &key)
  (add-header stream "Transfer-Encoding" "chunked"))

(defmethod trivial-gray-streams:stream-force-output :after ((stream response-stream))
  (with-slots (fd) stream
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 0)))

(defmethod trivial-gray-streams:stream-force-output :after ((stream ssl-response-mixin))
  (with-slots (ssl-stream) stream
    (force-output ssl-stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream content-length-response-stream))
  (with-slots (fd buffer) stream
    (loop for i in (queue-head buffer)
          do (write-to-response stream i (length i)))))

(defmethod trivial-gray-streams:stream-force-output ((stream chunked-response-stream))
  (%write-last-chunk stream))

(defmethod trivial-gray-streams:stream-force-output :before ((stream response-stream))
  (unless (started-p stream)
    (start-response stream)))

(defmethod trivial-gray-streams:stream-write-sequence :before ((response chunked-response-stream)
                                                               sequence start end &key)
  (unless (started-p response)
    (start-response response)))

(defmethod trivial-gray-streams:stream-write-sequence ((response content-length-response-stream)
                                                       (string string) start end &key)
  (with-slots (buffer) response
    (enqueue buffer (string-to-octets string :external-format (external-format-of response)
                                             :start start
                                             :end end))))

(defmethod trivial-gray-streams:stream-write-sequence ((response chunked-response-stream)
                                                       (string string) start end &key)
  (cffi:with-foreign-string ((s length) string
                             :encoding (external-format-of response)
                             :null-terminated-p nil
                             :start start :end end)
    (write-chunk response s length)))

(defmethod trivial-gray-streams:stream-write-sequence ((response content-length-response-stream)
                                                       sequence start end &key)
  (with-slots (buffer) response
    (enqueue buffer (octet-vector (subseq sequence start end)))))

(defmethod trivial-gray-streams:stream-write-sequence ((response chunked-response-stream)
                                                       sequence start end &key)
  (cffi-sys:with-pointer-to-vector-data (ptr sequence)
    (cffi-sys:inc-pointer ptr start)
    (write-chunk response ptr (- end start))))

(defmethod trivial-gray-streams:stream-write-sequence ((response ssl-chunked-response-stream)
                                                       sequence start end &key)
  (write-chunk response (subseq sequence start end) (- end start)))

(defmethod trivial-gray-streams:stream-write-string :before ((response chunked-response-stream) string
                                                             &optional start end)
  (declare (ignore start end))
  (unless (started-p response)
    (start-response response)))

(defmethod trivial-gray-streams:stream-write-string ((response content-length-response-stream)
                                                     (string string)
                                                     &optional (start 0) end)
  (trivial-gray-streams:stream-write-sequence response string start end))

(defmethod trivial-gray-streams:stream-write-string ((response chunked-response-stream) (string string)
                                                     &optional (start 0) end)
  (cffi:with-foreign-string ((s length) string
                             :encoding (external-format-of response)
                             :null-terminated-p nil
                             :start start :end (or end (length string)))
    (write-chunk response s length)))

(defmethod trivial-gray-streams:stream-write-char ((response response-stream) character)
  (trivial-gray-streams:stream-write-sequence response (make-string 1 :initial-element character)
                                              0 1))


(defmethod response-message-of ((response response-stream))
  (ecase (response-status-of response)
    (100 "Continue")
    (101 "Switching Protocols")
    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoritative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (306 "(Unused)")
    (307 "Temporary Redirect")
    (400 "Bad Request")
    (401 "Unauthorized")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Request Entity Too Large")
    (414 "Request-URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Requested Range Not Satisfiable")
    (417 "Expectation Failed")
    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")))

(defmethod start-response-line ((response response-stream))
  (with-slots (fd protocol external-format) response
    (format-to-response response external-format
                        "~a ~d ~a~a"
                        protocol
                        (response-status-of response)
                        (response-message-of response)
                        +crlf+)))

(defmethod start-response-header ((response response-stream))
  (with-slots (fd external-format) response
    (iterate (((k v) (scan-alist (response-headers-of response))))
      (format-to-response response external-format
                          "~a: ~a~a" k v +crlf+))
    (format-to-response response external-format "~a" +crlf+)))

(defmethod start-response ((response response-stream))
  (with-slots (fd) response
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 1)
    (start-response-line response)
    (start-response-header response)
    (setf (started-p response) t)))

(defmethod start-response :before ((response content-length-response-stream))
  (with-slots (buffer) response
    (add-header response "Content-Length" (loop for i in (queue-head buffer) sum (length i)))))


(defgeneric format-to-response (response external-format format &rest args))

(defmethod format-to-response ((response response-stream) external-format format &rest args)
  (with-slots (fd) response
    (cffi:with-foreign-string ((s length) (apply #'format nil format args)
                             :encoding external-format
                             :null-terminated-p nil)
      (sys-write fd s length))))

(defmethod format-to-response ((response ssl-response-mixin) external-format format &rest args)
  (with-slots (ssl-stream) response
    (write-sequence (string-to-octets (apply #'format nil format args)) ssl-stream)))


(defmethod add-header ((stream response-stream) key value)
  (push (cons key value) (response-headers-of stream)))

(defmethod unauthorized ((stream response-stream) &optional (realm "Common Lisp"))
  (setf (response-status-of stream) 401)
  (add-header stream "WWW-Authenticate" (format nil "Basic realm=\"~a\"" realm))
  (format stream "<head><title>401</title></head><body>Authorization Required</body>"))

(defmethod redirect ((stream response-stream) url)
  (setf (response-status-of stream) 302)
  (add-header stream "Location" url)
  (format stream "<head><title>302</title></head><body><a href=\"~a\">here</a></body>"
          (h url)))

(defmethod redirect-permanently ((stream response-stream) url)
  (setf (response-status-of stream) 301)
  (add-header stream "Location" url)
  (format stream "<head><title>301</title></head><body><a href=\"~a\">here</a></body>"
          (h url)))


(declaim (inline %write-chunk))
(defun %write-chunk (response buffer size)
  (unless (zerop size)
    (format-to-response response :latin-1 "~X~a" size +crlf+)
    (write-to-response response buffer size)
    (format-to-response response :latin-1 +crlf+)))

(declaim (inline %write-last-chunk))
(defun %write-last-chunk (response)
  (format-to-response response :latin-1 "0~a~a" +crlf+ +crlf+))

(declaim (inline write-chunk))
(defun write-chunk (response buffer size)
  (unless (started-p response)
    (start-response response))
  (%write-chunk response buffer size))
