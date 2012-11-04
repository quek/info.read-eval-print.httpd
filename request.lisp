(in-package :info.read-eval-print.httpd)

(defgeneric keep-alive-p (request))
(defgeneric make-response-stream (request))

(defclass request ()
  ((env :initform (make-hash-table :test #'equal) :accessor env-of)
   (fd :initarg :fd :accessor fd-of)
   (keep-alive-timer :initform nil)
   (parse-function :initform nil)
   (remain-request-buffer :initform nil)
   (accept-thread-fd)
   (params)))

(defmethod reset-request ((request request))
  (with-slots (env parse-function remain-request-buffer) request
    (clrhash env)
    (setf parse-function nil)
    (setf remain-request-buffer nil)
    (slot-makunbound request 'params)))

(defclass http-0.9-request (request)
  ())

(defclass http-1.0-request (request)
  ())

(defclass http-1.1-request (request)
  ())


(defmethod make-response-stream (request)
  (make-instance 'response-stream :fd (slot-value request 'fd)))

(defmethod env ((request request) key &optional default-value)
  (with-slots (env) request
    (gethash key env default-value)))

(defmethod (setf env) (value (request request) key)
  (with-slots (env) request
    (setf (gethash key env) value)))

(defmethod keep-alive-p (request)
  nil)

(defmethod keep-alive-p ((request http-1.1-request))
  ;; TODO Connection ヘッダ
  (with-slots (env) request
    (and
     (plusp (slot-value *server* 'keep-alive-timeout)))))
