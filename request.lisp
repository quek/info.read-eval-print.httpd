(in-package :info.read-eval-print.httpd)

(defclass request ()
  ((env :initform (make-hash-table :test #'equal) :accessor env-of)
   (fd :initarg :fd)
   (keep-alive-timer :initform nil)
   (parse-function :initform nil)
   (remain-request-buffer :initform nil)
   (accept-thread-fd)
   (response)))

(defmethod env ((request request) key &optional default-value)
  (with-slots (env) request
    (gethash key env default-value)))

(defmethod (setf env) (value (request request) key)
  (with-slots (env) request
    (setf (gethash key env) value)))

(defun keep-alive-p (request)
  ;; TODO Connection ヘッダ
  (with-slots (env) request
    (and
     (equal (env request :server-protocol) "HTTP/1.1")
     (plusp (slot-value *server* 'keep-alive-timeout)))))

(defmethod reset-request ((request request))
  (with-slots (env parse-function remain-request-buffer) request
    (clrhash env)
    (setf parse-function nil)
    (setf remain-request-buffer nil)))