(in-package :info.read-eval-print.httpd)

(defparameter *app-error-handler* 'default-app-error-handler)

(defgeneric call (app request))
(defgeneric app-handler-thread-fds (app-handler))

(defclass app-handler ()
  ((apps :initarg :apps :initform ())
   (number-of-threads :initarg :number-of-threads :initform 2)
   (threads :initform nil)
   (mailbox :initform (sb-concurrency:make-mailbox))))

(defmethod handle-request or ((handler app-handler) server request)
  (with-slots (mailbox apps) handler
    (awhen (assoc (env request :path-info) apps
                  :test (lambda (a b)
                          (ppcre:scan (format nil "^~a([/?].*|$)" (ppcre:quote-meta-chars b))
                                      a)))
      (with-slots (pipe-write-fd fd response close-function) request
        (setf (env request :script-name) (car it)
              (env request :path-info) (subseq (env request :path-info) (length (car it)))
              (env request :application) (cdr it)
              (isys:fd-nonblock fd) nil)
        (sb-concurrency:send-message mailbox request)
        :detach))))

(defmethod install-application ((handler app-handler) (application symbol) &key context-root)
  (install-application handler (make-instance application) :context-root context-root))

(defmethod install-application ((handler app-handler) application &key context-root)
  (with-slots (apps) handler
    (setf apps (acons context-root application apps))))

(defun return-client-fd (client-fd pipe-write-fd)
  (cffi:with-foreign-object (buf :int)
    (setf (cffi:mem-aref buf :int 0) client-fd)
    (isys:write pipe-write-fd buf #.(cffi:foreign-type-size :int))))

(defun app-handler-loop (mailbox)
  (loop for request = (sb-concurrency:receive-message mailbox)
        do (with-slots (pipe-write-fd fd) request
             (let ((*standard-output* (make-response-stream request)))
               (handler-case
                   (progn
                     (call (env request :application) request)
                     (force-output *standard-output*)
                     (reset-request request)
                     (setf (isys:fd-nonblock fd) t)
                     (return-client-fd fd pipe-write-fd))
                 (iolib.syscalls::epipe ()
                   (iolib.syscalls:close fd))
                 (error (e)
                   (iolib.syscalls:close fd)
                   (funcall *app-error-handler* e request *standard-output*)))))))

(defun default-app-error-handler (error request response)
  (declare (ignore request response))
  (format *error-output* "~a" error)
  (cerror "default-app-error-handler" error))

(defun ignore-app-error-handler (error request response)
  (declare (ignore request response))
  (warn (format nil "~a" error)))

(defmethod initialize-instance :after ((self app-handler) &key)
  (with-slots (number-of-threads mailbox threads) self
    (setf threads
          (collect (sb-thread:make-thread
                    #'app-handler-loop
                    :name (format nil "app thread ~d" (scan-range :length number-of-threads))
                    :arguments (list mailbox))))))

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
   (info.read-eval-print.httpd:start
    (make-instance 'info.read-eval-print.httpd:server
                   :application '(env-dump-app :context-root "/env")))))
|#
