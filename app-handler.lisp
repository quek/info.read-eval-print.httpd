(in-package :info.read-eval-print.httpd)

(defparameter *app-error-handler* 'default-app-error-handler)

(defgeneric call (app client))
(defgeneric app-handler-thread-fds (app-handler))

(defclass app-handler ()
  ((apps :initarg :apps :initform ())
   (number-of-threads :initarg :number-of-threads :initform 2)
   (threads :initform nil)
   (mailbox :initform (sb-concurrency:make-mailbox))))

(defmethod handle-request or ((handler app-handler) server client)
  (with-slots (mailbox apps) handler
    (awhen (assoc (env client :path-info) apps
                  :test (lambda (a b)
                          (ppcre:scan (format nil "^~a([/?].*|$)" (ppcre:quote-meta-chars b))
                                      a)))
      (with-slots (fd) client
        (setf (env client :script-name) (car it)
              (env client :path-info) (subseq (env client :path-info) (length (car it)))
              (env client :application) (cdr it)
              (isys:fd-nonblock fd) nil)
        (sb-concurrency:send-message mailbox client)
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
  (loop for client = (sb-concurrency:receive-message mailbox)
        do (with-slots (fd pipe-write-fd) client
             (let ((*standard-output* client))
               (handler-case
                   (progn
                     (call (env client :application) client)
                     (force-output *standard-output*)
                     (if (keep-alive-p client)
                         (progn
                           (reset-client client)
                           (setf (isys:fd-nonblock fd) t)
                           (return-client-fd fd pipe-write-fd))
                         (close client)))
                 (iolib.syscalls::epipe ()
                   (iolib.syscalls:close fd))
                 (error (e)
                   (iolib.syscalls:close fd)
                   (funcall *app-error-handler* e client *standard-output*)))))))

(defun default-app-error-handler (error client response)
  (declare (ignore client response))
  (format *error-output* "~a" error)
  (cerror "default-app-error-handler" error))

(defun ignore-app-error-handler (error client response)
  (declare (ignore client response))
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

(defmethod call ((aap env-dump-app) client)
  (write-string "<html>
<head><title>env-dump-app</title></head>
<body>
  <ul>")
   (iterate (((k v) (scan-hash (env-of client))))
     (format t "<li>~a = ~a</li>" k v))
  (write-string "</ul>")
  (print (h (princ-to-string client)))
  (write-string "</body></html>"))


#|
(sb-thread:make-thread
 (lambda ()
   (info.read-eval-print.httpd:start
    (make-instance 'info.read-eval-print.httpd:server
                   :application '(env-dump-app :context-root "/env")))))
|#
