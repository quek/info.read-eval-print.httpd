(in-package :info.read-eval-print.httpd)

(defclass app-handler ()
  ((app :initarg :app :initform (make-instance 'env-dump-app))))

(defgeneric call (handler env))

(defmethod handle-request or ((handler app-handler) server fd env)
  (let ((*standard-output* (make-instance 'response-stream :fd fd)))
    (call (slot-value handler 'app) *env*)
    (force-output *standard-output*)
    t))

(defclass env-dump-app ()
  ())

(defmethod call ((aap env-dump-app) env)
  (write-string "<html>
<head><title>env-dump-app</title></head>
<body>
  <ul>")
   (iterate (((k v) (scan-hash  env)))
     (format t "<li>~a = ~a</li>" k v))
  (write-string "
  </ul>
</body>
</html>"))

#|
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server
                                                 :handler (make-instance 'app-handler)))
|#
