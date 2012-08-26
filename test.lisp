(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :hu.dwim.stefil+hu.dwim.def)
  (ql:quickload :info.read-eval-print.httpd))

(info.read-eval-print.series-ext:sdefpackage :info.read-eval-print.httpd.test
  (:use :cl :info.read-eval-print.httpd))

(in-package :info.read-eval-print.httpd.test)

(hu.dwim.stefil:defsuite* (info.read-eval-print.httpd.test
                           :in hu.dwim.stefil:root-suite))


(defparameter *test-document-root*
  (string-right-trim '(#\/)
                     (directory-namestring
                      (merge-pathnames "www/" #.(or *compile-file-truename*
                                                    *load-truename*)))))
(defvar *test-server*)

(defun start-test-server (&optional (handler (make-instance 'info.read-eval-print.httpd:default-handler)))
  (setf *test-server*
        (make-instance 'info.read-eval-print.httpd:server
                       :document-root *test-document-root*
                       :handler handler))
  (sb-thread:make-thread
   (lambda ()
     (info.read-eval-print.httpd:start *test-server*))
   :name "test httpd thread")
  *test-server*)

(defun stop-test-sever ()
  (setf (info.read-eval-print.httpd:quit-p *test-server*) t))




