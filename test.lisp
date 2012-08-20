(in-package :info.read-eval-print.httpd)

(sb-thread:make-thread
 (lambda ()
   (info.read-eval-print.httpd:start
    (make-instance 'info.read-eval-print.httpd:server
                   :document-root (string-right-trim
                                   '(#\/)
                                   (directory-namestring
                                    (merge-pathnames "www/" #.(or *compile-file-truename*
                                                                  *load-truename*))))))))


