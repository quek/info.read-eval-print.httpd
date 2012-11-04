(in-package :info.read-eval-print.httpd)

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
      (with-slots (accept-thread-fd response) request
        (setf (env request :script-name) (car it)
              (env request :path-info) (subseq (env request :path-info) (length (car it)))
              (env request :application) (cdr it)
              accept-thread-fd *accept-thread-fd*)
        (describe request)
        (sb-concurrency:send-message mailbox request)
        (setf *detach-p* t)
        t))))

(defmethod install-application ((handler app-handler) (application symbol) &key context-root)
  (install-application handler (make-instance application) :context-root context-root))

(defmethod install-application ((handler app-handler) application &key context-root)
  (with-slots (apps) handler
    (setf apps (acons context-root application apps))))

(defun return-client-fd (client-fd accept-thread-fd)
  (cffi:with-foreign-object (buf :int)
    (setf (cffi:mem-aref buf :int 0) client-fd)
    (isys:write accept-thread-fd buf #.(cffi:foreign-type-size :int))))

(defun app-handler-loop (mailbox)
  (loop for request = (sb-concurrency:receive-message mailbox)
        do (describe request)
        do (with-slots (accept-thread-fd fd) request
             (let ((*standard-output* (make-response-stream request)))
               (call (env request :application) request)
               (force-output *standard-output*)
               (reset-request request)
               (return-client-fd fd accept-thread-fd)))))

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

(defun params (request name)
  (with-slots (params) request
    (unless (slot-boundp request 'params)
      (setf params
            (let ((query-string (env request :query-string)))
              (when (and query-string (plusp (length query-string)))
               (collect 'bag (let ((k-v (ppcre:split "=" (scan-split "&" query-string))))
                               (cons (percent:decode (car k-v))
                                     (percent:decode (cadr k-v)))))))))
    (cdr (assoc name params :test #'string-equal))))

#|
(sb-thread:make-thread
 (lambda ()
   (info.read-eval-print.httpd:start
    (make-instance 'info.read-eval-print.httpd:server
                   :application '(env-dump-app :context-root "/env")))))
|#
