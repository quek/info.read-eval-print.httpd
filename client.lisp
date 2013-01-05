(in-package :info.read-eval-print.httpd)

(defgeneric reset-client (client))
(defgeneric receive-from (client buffer start end))
(defgeneric env (client key &optional default-value))
(defgeneric (setf env) (value client key))
(defgeneric keep-alive-p (client))
(defgeneric authorization (client))
(defgeneric external-format-of (stream))
(defgeneric (setf external-format-of) (external-format stream))
(defgeneric start-response (client stream))
(defgeneric add-header (client key value))
(defgeneric delete-header (client key))


(define-condition io-block-error () ())


(defclass stream-mixin (trivial-gray-streams:trivial-gray-stream-mixin
                        trivial-gray-streams:fundamental-binary-input-stream
                        trivial-gray-streams:fundamental-binary-output-stream)
  ((external-format :initarg :external-format :initform :utf-8 :accessor external-format-of)))

(defclass http-client-mixin ()
  ())

(defclass https-client-mixin ()
  ())

(defun default-response-headers ()
  (list (cons "Date" (rfc-2822-now))
        (cons "Content-Type" "text/html; charset=UTF-8")))

(defclass client-mixin (stream-mixin)
  ((server :initarg :server)
   (env :initform (make-hash-table :test #'equal) :accessor env-of)
   (fd :initarg :fd :accessor fd-of)
   (stream)
   (keep-alive-timer :initform nil)
   (parse-function :initform nil)
   (remain-request-buffer :initform nil)
   (post-data :initform nil)
   (params)
   (pipe-write-fd :initarg :pipe-write-fd)
   (response-status :initarg :response-status :initform 200 :accessor response-status-of)
   (response-headers :initarg :respone-headens :initform (default-response-headers))
   (started :initform nil :accessor started-p)))

(defclass http-client (http-client-mixin client-mixin)
  ())
(defclass https-client (https-client-mixin client-mixin)
  ())

(defclass http-0.9-mixin () ())
(defclass http-1.0-mixin () ())
(defclass http-1.1-mixin () ())

(defclass http-0.9-client (http-0.9-mixin http-client) ())
(defclass http-1.0-client (http-1.0-mixin http-client) ())
(defclass http-1.1-client (http-1.1-mixin http-client) ())

(defclass https-0.9-client (http-0.9-mixin https-client) ())
(defclass https-1.0-client (http-1.0-mixin https-client) ())
(defclass https-1.1-client (http-1.1-mixin https-client) ())


(defmethod reset-client ((client client-mixin))
  (with-slots (env parse-function remain-request-buffer
               post-data stream
               response-status response-headers started) client
    (clrhash env)
    (setf parse-function nil)
    (setf remain-request-buffer nil)
    (setf post-data nil)
    (setf stream (raw-stream stream))
    (slot-makunbound client 'params)
    (setf response-status 200)
    (setf response-headers (default-response-headers))
    (setf started nil)))


(defmethod initialize-instance :after ((self http-client-mixin) &key fd)
  (with-slots (stream external-format) self
    (setf stream (make-instance 'fd-stream
                                :fd fd
                                :client self
                                :external-format external-format))))

(defmethod initialize-instance :after ((self https-client-mixin) &key fd ssl-cert ssl-key)
  (with-slots (stream external-format) self
    (setf stream (make-instance 'ssl-stream
                                :client self
                                :stream (cl+ssl:make-ssl-server-stream fd
                                                                       :certificate ssl-cert
                                                                       :key ssl-key)
                                :external-format external-format))))


(defmethod update-instance-for-different-class :after ((old client-mixin)
                                                       (new http-0.9-mixin)
                                                       &key)
  (setf (slot-value new 'stream)
        (make-instance 'content-length-stream :stream (slot-value old 'stream))))

(defmethod update-instance-for-different-class :after ((old client-mixin)
                                                       (new http-1.0-mixin)
                                                       &key)
  (setf (slot-value new 'stream)
        (make-instance 'content-length-stream :stream (slot-value old 'stream))))

(defmethod update-instance-for-different-class :after ((old client-mixin)
                                                       (new http-1.1-mixin)
                                                       &key)
  (setf (slot-value new 'stream)
        (make-instance 'chunked-stream :stream (slot-value old 'stream))))


(defmethod env ((client client-mixin) key &optional default-value)
  (with-slots (env) client
    (gethash key env default-value)))

(defmethod (setf env) (value (client client-mixin) key)
  (with-slots (env) client
    (setf (gethash key env) value)))


(defmethod add-header ((client client-mixin) key value)
  (with-slots (response-headers) client
    (push (cons key value) response-headers)))

(defmethod delete-header ((client client-mixin) key)
  (with-slots (response-headers) client
    (setf response-headers (delete key response-headers :key #'car :test #'equal))))

(defmethod keep-alive-p ((client client-mixin))
  nil)

(defmethod keep-alive-p ((client http-1.1-mixin))
  (with-slots (server env) client
    (and
     (keep-alive-p server)
     ;; TODO Connection ヘッダ
     (plusp (slot-value server 'keep-alive-timeout)))))


(defmethod authorization ((client client-mixin))
  (let ((authorization (env client :authorization)))
    (ppcre:register-groups-bind (data) ("Basic \(.*\)" authorization)
      (let ((user-password (base64:base64-string-to-string data)))
        (awhen (position #\: user-password)
          (values (subseq user-password 0 it)
                  (subseq user-password (1+ it))))))))


(defun params (client name)
  (with-slots (params) client
    (unless (slot-boundp client 'params)
      (setf params
            (append
             (let ((query-string (env client :query-string)))
               (when (and query-string (plusp (length query-string)))
                 (collect 'bag (let ((k-v (ppcre:split "=" (scan-split "&" query-string))))
                                 (cons (percent:decode (car k-v))
                                       (percent:decode (cadr k-v)))))))
             (let ((post-data (slot-value client 'post-data))
                   (params nil))
               (when (and post-data (plusp (length post-data)))
                 (labels ((f (seq)
                            (let ((p= (position (char-code #\=) seq)))
                              (cons (percent:decode (octets-to-string seq :end p=))
                                    (percent:decode (octets-to-string seq :start (1+ p=)))))))
                   (loop for start = 0 then (1+ p&)
                         for p& = (position (char-code #\&) post-data :start start)
                         do (if p&
                                (push (f (subseq post-data start p&)) params)
                                (progn
                                  (push (f (subseq post-data start)) params)
                                  (return (nreverse params)))))))))))
    (cdr (assoc name params :test #'string-equal))))

(defmacro with-params ((client) &body body)
  (labels ((@p (thing)
             (and (symbolp thing)
                  (plusp (length (symbol-name thing)))
                  (string= thing "@" :end1 1)))
           (key (symbol)
             (intern (subseq (symbol-name symbol) 1) :keyword))
           (f (form)
             (cond ((@p form)
                    `(params ,client ,(key form)))
                   ((atom form)
                    form)
                   (t
                    (cons (f (car form)) (f (cdr form)))))))
    `(progn ,@(f body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; gray stream

(defmethod trivial-gray-streams:stream-write-char ((stream stream-mixin) character)
  (let ((seq (string-to-octets (string character) :external-format (external-format-of stream))))
    (trivial-gray-streams:stream-write-sequence stream seq 0 (length seq))))

(defmethod trivial-gray-streams:stream-line-column ((stream stream-mixin))
  nil)


(defmethod trivial-gray-streams:stream-read-sequence ((client client-mixin)
                                                      sequence start end &key)
  (with-slots (stream) client
    (trivial-gray-streams:stream-read-sequence stream sequence start end)))

(defmethod trivial-gray-streams:stream-write-sequence ((client client-mixin)
                                                       sequence start end &key)
  (with-slots (stream) client
    (trivial-gray-streams:stream-write-sequence stream sequence start end)))

(defmethod trivial-gray-streams:stream-force-output ((client client-mixin))
  (with-slots (stream) client
    (trivial-gray-streams:stream-force-output stream)))


(declaim (inline write-ptr))
(defgeneric write-ptr (stream ptr size))
(defgeneric fd-of (stream))

(defclass abstract-stream (stream-mixin)
  ((client :initarg :client)))

(defclass fd-stream (abstract-stream)
  ((fd :initarg :fd)))

(defclass ssl-stream (abstract-stream)
  ((stream :initarg :stream)))

(defclass stream-wrapper (stream-mixin)
  ((stream :initarg :stream)))

(defclass content-length-stream (stream-wrapper)
  ((buffer :initform (make-queue))))

(defclass chunked-stream (stream-wrapper)
  ((buffer :initform (make-queue))
   (buffer-size :initform 0)))


(defmethod close ((client client-mixin) &key abort)
  (with-slots (stream) client
    (close stream :abort abort)))

(defmethod close ((stream fd-stream) &key abort)
  (with-slots (fd) stream
    (if abort
        (ignore-errors #1=(iolib.syscalls:close fd))
        #1#)))

(defmethod close ((stream ssl-stream) &key abort)
  (with-slots (stream) stream
    (close stream :abort abort)))

(defmethod close ((stream stream-wrapper) &key abort)
  (with-slots (stream) stream
    (close stream :abort abort)))


(defgeneric raw-stream (stream)
  (:method ((stream stream-wrapper))
    (raw-stream (slot-value stream 'stream)))
  (:method ((stream client-mixin))
    (raw-stream (slot-value stream 'stream)))
  (:method (stream)
           stream))


(defmethod client-of ((stream abstract-stream))
  (slot-value stream 'client))

(defmethod client-of ((stream stream-wrapper))
  (with-slots (stream) stream
    (client-of stream)))

(defmethod add-header ((stream abstract-stream) key value)
  (with-slots (client) stream
    (add-header client key value)))

(defmethod add-header ((stream stream-wrapper) key value)
  (with-slots (stream) stream
    (add-header stream key value)))



(defmethod fd-of ((stream fd-stream))
  (slot-value stream 'fd))

(defmethod fd-of (stream)
  (fd-of (slot-value stream 'stream)))


(defmethod external-format-of (stream)
  (with-slots (stream) stream
    (external-format-of stream)))

(defmethod (setf external-format-of) (external-format stream)
  (with-slots (stream) stream
    (setf (external-format-of stream) external-format)))


(defmethod write-ptr ((stream fd-stream) ptr size)
  (with-slots (fd) stream
    (sys-write fd ptr size)))


(defmethod trivial-gray-streams:stream-read-sequence ((stream fd-stream) sequence
                                                      start end &key)
  (with-slots (fd) stream
    (handler-case (receive fd sequence start end)
      (iolib.syscalls:econnreset () -1))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream ssl-stream) sequence
                                                      start end &key)
  (with-slots (stream) stream
    ;; TODO cl+ssl の API で nonblock な read-sequence ができないので
    #+nil
    (read-sequence buffer ssl-estream :start start :end end)
    (let ((count 0)
          (size (- end start)))
      (loop for byte = (and (< count size)
                            (listen stream)
                            (read-byte stream))
            while byte
            do (setf (aref sequence start) byte)
               (incf start)
               (incf count))
      (if (and (zerop count) (plusp size))
          (error 'io-block-error)
          count))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream stream-wrapper) sequence
                                                      start end &key)
  (trivial-gray-streams:stream-read-sequence (raw-stream stream) sequence start end))


(defmethod trivial-gray-streams:stream-write-sequence :before ((stream chunked-stream)
                                                               sequence start end &key)
  (start-response (client-of stream) stream))

(defmethod trivial-gray-streams:stream-write-sequence ((stream fd-stream)
                                                       sequence start end &key)
  (cffi-sys:with-pointer-to-vector-data (ptr sequence)
    (cffi:incf-pointer ptr start)
    (write-ptr stream ptr (- end start))))

(defmethod trivial-gray-streams:stream-write-sequence ((stream fd-stream)
                                                       (string string) start end &key)
  (cffi:with-foreign-string ((ptr size) string :start start :end end
                                               :encoding (external-format-of stream)
                                               :null-terminated-p nil)
    (write-ptr stream ptr size)))

(defmethod trivial-gray-streams:stream-write-sequence ((ssl ssl-stream)
                                                       sequence start end &key)
  ;; (print (octets-to-string sequence) *terminal-io*)
  (with-slots (stream) ssl
    (trivial-gray-streams:stream-write-sequence stream sequence start end)))

(defmethod trivial-gray-streams:stream-write-sequence ((ssl ssl-stream)
                                                       (string string) start end &key)
  ;; (print string *terminal-io*)
  (with-slots (stream) ssl
    (let ((seq (string-to-octets string :external-format (external-format-of ssl)
                                        :start start
                                        :end end)))
      (trivial-gray-streams:stream-write-sequence stream seq 0 (length seq)))t))

(defmethod trivial-gray-streams:stream-write-sequence ((stream content-length-stream)
                                                       sequence start end &key)
  (with-slots (buffer) stream
    (enqueue buffer (subseq sequence start end))))

(defmethod trivial-gray-streams:stream-write-sequence ((self content-length-stream)
                                                       (string string) start end &key)
  (with-slots (buffer) self
    (enqueue buffer (string-to-octets string :external-format (external-format-of self)
                                             :start start
                                             :end end))))

(defmethod trivial-gray-streams:stream-write-sequence ((self chunked-stream)
                                                       sequence start end &key)
  (let ((size (- end start)))
    (when (plusp size)
      (with-slots (buffer buffer-size) self
        (enqueue buffer (list sequence start end))
        (incf buffer-size size)))))

(defmethod trivial-gray-streams:stream-write-sequence ((self chunked-stream)
                                                       (string string) start end &key)
  (with-slots (buffer buffer-size) self
    (let* ((seq (string-to-octets string :external-format (external-format-of self)
                                         :start start :end end))
           (size (length seq)))
      (when (plusp size)
        (enqueue buffer (list seq 0 size))
        (incf buffer-size size)))))


(defmethod trivial-gray-streams:stream-force-output :after ((client client-mixin))
  (with-slots (fd) client
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 0)))

(defmethod trivial-gray-streams:stream-force-output ((self ssl-stream))
  (with-slots (stream) self
    (force-output stream)))

(defmethod trivial-gray-streams:stream-force-output ((self content-length-stream))
  (start-response (client-of self) self)
  (with-slots (stream buffer) self
    (loop for seq in (queue-head buffer)
          for len = (length seq)
          if (plusp len)
            do (trivial-gray-streams:stream-write-sequence stream seq 0 len))
    (force-output stream)))

(defmethod trivial-gray-streams:stream-force-output ((self chunked-stream))
  (with-slots (stream buffer buffer-size) self
    (unless (zerop buffer-size)
      (let ((array (make-octet-vector buffer-size)))
        (loop for start1 = 0 then (+ start1 (- end2 start2))
              for (buf start2 end2) in (queue-head buffer)
              do (replace array buf :start1 start1 :start2 start2 :end2 end2))
        (format stream "~X~a" buffer-size +crlf+)
        (trivial-gray-streams:stream-write-sequence stream array 0 buffer-size)
        (format stream +crlf+)))
    (format stream "0~a~a" +crlf+ +crlf+)
    (force-output stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start response

(defmethod response-message (response-status)
  (ecase response-status
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


(defmethod start-response-line ((client client-mixin))
  (with-slots (stream response-status) client
    (format (raw-stream stream)
            "~a ~d ~a~a"
            (env client :server-protocol)
            response-status
            (response-message response-status)
            +crlf+)))

(defmethod start-response-header ((client client-mixin))
  (with-slots (stream response-headers) client
    (let ((stream (raw-stream stream)))
      (iterate (((k v) (scan-alist response-headers)))
        (format stream "~a: ~a~a" k v +crlf+))
      (format stream "~a" +crlf+))))

(defmethod start-response :around ((client client-mixin) stream)
  (unless (started-p client)
    (setf (started-p client) t)
    (call-next-method)))

(defmethod start-response ((client client-mixin) stream)
  (with-slots (fd) client
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 1)
    (start-response-line client)
    (start-response-header client)))

(defmethod start-response :before ((client client-mixin) (stream content-length-stream))
  (with-slots (buffer) stream
    (add-header client "Content-Length" (loop for i in (queue-head buffer)
                                              sum (length i)))))

(defmethod start-response :before ((client client-mixin) (stream chunked-stream))
  (add-header client "Transfer-Encoding" "chunked"))
