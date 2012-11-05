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
   (post-data :initform nil)
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

(defmethod authorization ((request request))
  (let ((authorization (env request :http-authorization)))
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
                            (let ((p= (position #\= seq)))
                              (cons (percent:decode (octets-to-string seq :end p=))
                                    (percent:decode (octets-to-string seq :start (1+ p=)))))))
                   (loop for start = 0 then (1+ p&)
                         for p& = (position #\& post-data :start start)
                         do (if p&
                                (push (f (subseq post-data start p&)) params)
                                (progn
                                  (push (f (subseq post-data start)) params)
                                  (return (nreverse params)))))))))))
    (cdr (assoc name params :test #'string-equal))))