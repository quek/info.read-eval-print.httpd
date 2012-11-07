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
  ((fd :initarg :fd :accessor fd-of)
   (external-format :initarg :external-format :initform :utf-8
                    :accessor external-format-of)
   (response-status :initarg :response-status :initform 200
                    :accessor response-status-of)
   (response-headers :initarg :respone-headens
                     :initform (list (cons "Date" (rfc-2822-now))
                                     (cons "Content-Type" "text/html; charset=UTF-8")
                                     (cons "Transfer-Encoding" "chunked"))
                     :accessor response-headers-of)
   (started :initform nil :accessor started-p)))

(defmethod trivial-gray-streams:stream-force-output ((stream response-stream))
  (unless (started-p stream)
    (start-response stream))
  (%write-last-chunk (fd-of stream)))

(defmethod trivial-gray-streams:stream-write-sequence :before ((stream response-stream)
                                                               sequence start end &key)
  (unless (started-p stream)
    (start-response stream)))

(declaim (inline %write-chunk))
(defun %write-chunk (fd buffer size)
  (unless (zerop size)
    (%format-to-fd fd :latin-1 "~X~a" size +crlf+)
    (isys:write fd buffer size)
    (%format-to-fd fd :latin-1 +crlf+)))

(declaim (inline %write-last-chunk))
(defun %write-last-chunk (fd)
  (%format-to-fd fd :latin-1 "0~a~a" +crlf+ +crlf+)
  (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 0))

(declaim (inline write-chunk))
(defun write-chunk (stream buffer size)
  (unless (started-p stream)
    (start-response stream))
  (%write-chunk (fd-of stream) buffer size))

(defmethod trivial-gray-streams:stream-write-sequence ((stream response-stream)
                                                       (string string) start end &key)
  (cffi:with-foreign-string ((s length) string
                             :encoding (external-format-of stream)
                             :null-terminated-p nil
                             :start start :end end)
    (write-chunk stream s length)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream response-stream)
                                                       sequence start end &key)
  (cffi-sys:with-pointer-to-vector-data (ptr sequence)
    (cffi-sys:inc-pointer ptr start)
    (write-chunk stream ptr (- end start))))

(defmethod trivial-gray-streams:stream-write-string :before ((stream response-stream) string
                                                             &optional start end)
  (declare (ignore start end))
  (unless (started-p stream)
    (start-response stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream response-stream) (string string)
                                                     &optional (start 0) end)
  (cffi:with-foreign-string ((s length) string
                             :encoding (external-format-of stream)
                             :null-terminated-p nil
                             :start start :end (or end (length string)))
    (write-chunk stream s length)))

(defmethod trivial-gray-streams:stream-write-char :before ((stream response-stream) character)
  (unless (started-p stream)
    (start-response stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream response-stream) character)
  (trivial-gray-streams:stream-write-sequence stream (make-string 1 :initial-element character)
                                              0 1))


(defmethod response-message-of ((stream response-stream))
  (ecase (response-status-of stream)
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

(defmethod start-response-line ((stream response-stream))
  (let ((fd (fd-of stream))
        (external-format (external-format-of stream)))
    (%format-to-fd fd external-format
                   "HTTP/1.1 ~d ~a~a"
                   (response-status-of stream)
                   (response-message-of stream)
                   +crlf+)))

(defmethod start-response-header ((stream response-stream))
  (let ((fd (fd-of stream))
        (external-format (external-format-of stream)))
    (iterate (((k v) (scan-alist (response-headers-of stream))))
      (%format-to-fd fd external-format
                     "~a: ~a~a" k v +crlf+))
    (%format-to-fd fd external-format +crlf+)))

(defmethod start-response ((stream response-stream))
  (let ((fd (fd-of stream)))
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 1)
    (start-response-line stream)
    (start-response-header stream)
    (setf (started-p stream) t)))


(defun %format-to-fd (fd exteral-format format &rest args)
  (cffi:with-foreign-string ((s length) (apply #'format nil format args)
                             :encoding exteral-format
                             :null-terminated-p nil)
    (isys:write fd s length)))

(defmethod add-header ((stream response-stream) key value)
  (push (cons key value) (response-headers-of stream)))

(defmethod unauthorized ((stream response-stream) &optional (realm "Common Lisp"))
  (setf (response-status-of stream) 401)
  (add-header stream "WWW-Authenticate" (format nil "Basic realm=\"~a\"" realm)))

(defmethod redirect ((stream response-stream) url)
  (setf (response-status-of stream) 302)
  (add-header stream "Location" url))

(defmethod redirect-permanently ((stream response-stream) url)
  (setf (response-status-of stream) 301)
  (add-header stream "Location" url))
