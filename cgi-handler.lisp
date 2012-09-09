(in-package :info.read-eval-print.httpd)

(defclass cgi-response-stream (response-stream)
  ())

(defclass cgi-handler (after-handle-request-mixin)
  ())

(defmethod handle-request or ((handler cgi-handler) server request)
  (let ((path (concatenate 'string (env request :document-root)
                           (env request :path-info))))
    (when (alexandria:ends-with-subseq ".cgi" path :test #'equal)
      (let* ((fd (fd-of request))
             (response-stream (make-instance 'cgi-response-stream :fd fd)))
        (let ((out (make-string-output-stream)))
         (sb-ext:run-program
          "/bin/sh" (list path)
          :output out
          :environment (collect 'bag
                         (choose
                          (multiple-value-bind (k v) (scan-hash (slot-value request 'env))
                            (when (stringp v)
                              (format nil "~a=~a" k v)))))
          :wait nil
          :status-hook (lambda (process)
                         (let ((s (get-output-stream-string out)))
                           (print (list :status-hook process s path))
                           (write-string s response-stream)
                           (force-output response-stream)
                           (reset-request request))
                         (return-client-fd fd *accept-thread-fd*))))
        #+nil
        (sb-ext:run-program
         "/bin/sh" (list path)
         :output response-stream
         :environment (collect 'bag
                        (choose
                         (multiple-value-bind (k v) (scan-hash (slot-value request 'env))
                           (when (stringp v)
                             (format nil "~a=~a" k v)))))
         :wait nil
         :status-hook (lambda (process)
                        (print (list :status-hook process))
                        (force-output response-stream)
                        (reset-request request)
                        (return-client-fd fd *accept-thread-fd*))))
      t)))

(defmethod start-response-header ((stream cgi-response-stream)))
