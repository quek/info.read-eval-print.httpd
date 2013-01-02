(in-package :info.read-eval-print.httpd)

(defgeneric keep-alive-p (server request))
(defgeneric make-response-stream (request))

(defclass request ()
  ((server :initarg :server)
   (env :initform (make-hash-table :test #'equal) :accessor env-of)
   (fd :initarg :fd :accessor fd-of)
   (keep-alive-timer :initform nil)
   (parse-function :initform nil)
   (remain-request-buffer :initform nil)
   (post-data :initform nil)
   (params)
   (pipe-write-fd :initarg :pipe-write-fd)
   (response :initform nil)))

(defmethod reset-request ((request request))
  (with-slots (env parse-function remain-request-buffer response) request
    (clrhash env)
    (setf parse-function nil)
    (setf remain-request-buffer nil)
    (slot-makunbound request 'params)
    (setf response nil)))

(defclass ssl-request-mixin ()
  ((ssl-stream)))

(defmethod initialize-instance :after ((self ssl-request-mixin) &key fd ssl-cert ssl-key)
  (with-slots (ssl-stream) self
    (setf ssl-stream (cl+ssl:make-ssl-server-stream fd
                                                    :certificate ssl-cert
                                                    :key ssl-key))))


(defclass ssl-request (request ssl-request-mixin)
  ())

(defclass http-0.9-request (request)
  ())

(defclass https-0.9-request (ssl-request)
  ())

(defclass http-1.0-request (request)
  ())

(defclass https-1.0-request (ssl-request)
  ())

(defclass http-1.1-request (request)
  ())

(defclass https-1.1-request (ssl-request)
  ())

(defgeneric receive-from (request buffer start end))

(defmethod receive-from (request buffer start end)
  (with-slots (fd) request
    (handler-case (receive fd buffer start end)
      (iolib.syscalls:econnreset () -1))))

(defmethod receive-from ((request ssl-request) buffer start end)
  (with-slots (ssl-stream) request
    #+nil
    (read-sequence buffer ssl-estream :start start :end end)
    (let ((count 0)
          (size (- end start)))
      (loop for byte = (and (< count size)
                            (listen ssl-stream)
                            (read-byte ssl-stream))
            while byte
            do (setf (aref buffer start) byte)
               (incf start)
               (incf count))
      (if (and (zerop count) (plusp size))
          nil                           ;ewouldblock
          count))))

(defmethod response-of (request)
  (with-slots (response) request
    (or response
        (setf response (make-response-stream request)))))

(defmethod make-response-stream (request)
  (make-instance 'content-length-response-stream
                 :fd (slot-value request 'fd)
                 :protocol (env request :server-protocol)
                 :request request))

(defmethod make-response-stream ((request http-1.1-request))
  (make-instance 'chunked-response-stream
                 :fd (slot-value request 'fd)
                 :protocol (env request :server-protocol)
                 :request request))

(defmethod make-response-stream ((request ssl-request))
  (with-slots (fd ssl-stream) request
   (make-instance 'ssl-content-length-response-stream
                  :fd fd
                  :protocol (env request :server-protocol)
                  :request request
                  :ssl-stream ssl-stream)))

(defmethod make-response-stream ((request https-1.1-request))
  (with-slots (fd ssl-stream) request
   (make-instance 'ssl-chunked-response-stream
                  :fd fd
                  :protocol (env request :server-protocol)
                  :request request
                  :ssl-stream ssl-stream)))

(defmethod env ((request request) key &optional default-value)
  (with-slots (env) request
    (gethash key env default-value)))

(defmethod (setf env) (value (request request) key)
  (with-slots (env) request
    (setf (gethash key env) value)))

(defmethod keep-alive-p (server request)
  nil)

(defmethod keep-alive-p (server (request http-1.1-request))
  ;; TODO Connection ヘッダ
  (with-slots (env) request
    (and
     (plusp (slot-value server 'keep-alive-timeout)))))

(defmethod authorization ((request request))
  (let ((authorization (env request :authorization)))
    (ppcre:register-groups-bind (data) ("Basic \(.*\)" authorization)
      (let ((user-password (base64:base64-string-to-string data)))
        (awhen (position #\: user-password)
          (values (subseq user-password 0 it)
                  (subseq user-password (1+ it))))))))


(defun params (request name)
  (with-slots (params) request
    (unless (slot-boundp request 'params)
      (setf params
            (append
             (let ((query-string (env request :query-string)))
               (when (and query-string (plusp (length query-string)))
                 (collect 'bag (let ((k-v (ppcre:split "=" (scan-split "&" query-string))))
                                 (cons (percent:decode (car k-v))
                                       (percent:decode (cadr k-v)))))))
             (let ((post-data (slot-value request 'post-data))
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

(defmacro with-params ((request) &body body)
  (labels ((@p (thing)
             (and (symbolp thing)
                  (plusp (length (symbol-name thing)))
                  (string= thing "@" :end1 1)))
           (key (symbol)
             (intern (subseq (symbol-name symbol) 1) :keyword))
           (f (form)
             (cond ((@p form)
                    `(params ,request ,(key form)))
                   ((atom form)
                    form)
                   (t
                    (cons (f (car form)) (f (cdr form)))))))
    `(progn ,@(f body))))
