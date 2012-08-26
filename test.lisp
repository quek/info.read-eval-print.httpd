(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :hu.dwim.stefil+hu.dwim.def)
  (ql:quickload :info.read-eval-print.httpd)
  (ql:quickload :drakma))

(setq drakma:*drakma-default-external-format* :utf-8)

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.httpd.test
 (:use :cl :info.read-eval-print.httpd)
 (:import-from :info.read-eval-print.httpd #:+crlf+))

(in-package :info.read-eval-print.httpd.test)

(hu.dwim.stefil:defsuite* (info.read-eval-print.httpd.test
                           :in hu.dwim.stefil:root-suite))


(defparameter *test-document-root*
  (string-right-trim '(#\/)
                     (directory-namestring
                      (merge-pathnames "www/" #.(or *compile-file-truename*
                                                    *load-truename*)))))

(defparameter *test-host* "localhost")
(defparameter *test-port* 19580)

(defun url (path)
  (format nil "http://~a:~d~a" *test-host* *test-port* path))

(defvar *test-server*)

(defun start-test-server (&optional (handler (make-instance 'info.read-eval-print.httpd:default-handler)))
  (setf *test-server*
        (make-instance 'info.read-eval-print.httpd:server
                       :document-root *test-document-root*
                       :handler handler
                       :port *test-port*))
  (sb-thread:make-thread
   (lambda ()
     (info.read-eval-print.httpd:start *test-server*))
   :name "test httpd thread")
  *test-server*)

(defun stop-test-sever ()
  (setf (info.read-eval-print.httpd:quit-p *test-server*) t))

(defun send-line (socket format &rest args)
  (apply #'format socket format args)
  (write-string +crlf+ socket)
  (force-output socket))

(defun receive-line (socket)
  (string-right-trim '(#\cr) (read-line socket)))

(defun path-content (path)
  (collect 'string
    (scan-file (concatenate 'string *test-document-root* path) #'read-char)))

(hu.dwim.stefil::defixture default-fixture
  (start-test-server)
  (sleep 0.1)
  (unwind-protect (hu.dwim.stefil:-body-)
    (stop-test-sever)
    (sleep 0.1)))

(hu.dwim.stefil:defsuite* (info.read-eval-print.httpd.test.http
                           :in info.read-eval-print.httpd.test)
    nil
  (hu.dwim.stefil:with-fixture default-fixture
    (hu.dwim.stefil:-run-child-tests-)))

(hu.dwim.stefil:deftest test-get ()
  (iolib.sockets:with-open-socket (socket :remote-host *test-host*
                                          :remote-port *test-port*
                                          :external-format :utf-8)
    (send-line socket "GET /index.html HTTP/1.1")
    (send-line socket "Host: localhost")
    (send-line socket "")
    (hu.dwim.stefil:is (string= "HTTP/1.1 200 OK" (receive-line socket))))
  (hu.dwim.stefil:is (string= (path-content "/index.html")
                              (drakma:http-request (url "/index.html")))))

(info.read-eval-print.httpd.test)
