(in-package :info.read-eval-print.httpd)

(defgeneric call (app request))
(defgeneric app-handler-thread-fds (app-handler))

(defclass app-handler ()
  ((app :initarg :app :initform (make-instance 'env-dump-app))
   (number-of-threads :initarg :number-of-threads :initform 2)
   (threads :initform nil)
   (mailbox :initform (sb-concurrency:make-mailbox))))

(defmethod handle-request or ((handler app-handler) server fd request)
  (with-slots (mailbox) handler
    (with-slots (accept-thread-fd response) request
      (setf accept-thread-fd *accept-thread-fd*)
      (setf response (make-instance 'response-stream :fd fd))
      (sb-concurrency:send-message mailbox request)
      t)))

(defun return-client-fd (client-fd accept-thread-fd)
  (cffi:with-foreign-object (buf :int)
    (setf (cffi:mem-aref buf :int 0) client-fd)
    (isys:write accept-thread-fd buf #.(cffi:foreign-type-size :int))))

(defun app-handler-loop (app mailbox)
  (loop for request = (sb-concurrency:receive-message mailbox :timeout 500)
        do (with-slots (accept-thread-fd fd response) request
             (let ((*standard-output* response))
               (call app request)
               (force-output *standard-output*)
               (reset-request request)
               (return-client-fd fd accept-thread-fd)))))


(defmethod initialize-instance :after ((self app-handler) &key)
  (with-slots (app number-of-threads mailbox threads) self
    (setf threads
          (collect (sb-thread:make-thread
                    #'app-handler-loop
                    :name (format nil "app thread ~d" (scan-range :length number-of-threads))
                    :arguments (list app mailbox))))))

(defclass env-dump-app ()
  ())

(defmethod call ((aap env-dump-app) request)
  (write-string "<html>
<head><title>env-dump-app</title></head>
<body>
  <ul>")
   (iterate (((k v) (scan-hash (env-of request))))
     (format t "<li>~a = ~a</li>" k v))
  (write-string "</ul>")
  (print (h (princ-to-string request)))
  (write-string "</body></html>"))

#|
(sb-thread:make-thread
 (lambda ()
   (info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server
                                                    :handler (make-instance 'app-handler)))))
|#
