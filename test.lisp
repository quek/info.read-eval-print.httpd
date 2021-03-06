(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :hu.dwim.stefil+hu.dwim.def)
  (ql:quickload :info.read-eval-print.httpd)
  (ql:quickload :info.read-eval-print.html)
  (ql:quickload :drakma))

(setq drakma:*drakma-default-external-format* :utf-8)

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.httpd.test
 (:use :cl :info.read-eval-print.httpd :info.read-eval-print.html)
 (:import-from :info.read-eval-print.httpd #:+crlf+)
 (:import-from :hu.dwim.stefil #:deftest #:is))

(in-package :info.read-eval-print.httpd.test)

(hu.dwim.stefil:defsuite* (info.read-eval-print.httpd.test
                           :in hu.dwim.stefil:root-suite))


(defparameter *dir* (pathname (directory-namestring #.(or *compile-file-truename* *load-truename*))))

(defparameter *test-document-root*
  (string-right-trim '(#\/)
                     (directory-namestring
                      (merge-pathnames "www/" *dir*))))

(defparameter *test-host* "localhost")
(defparameter *test-port* 19580)

(defun url (path)
  (format nil "http://~a:~d~a" *test-host* *test-port* path))

(defvar *test-server*)

(defclass test-app ()
  ())

(defun /hello ()
  (html (:html
          (:head (:title "hello"))
          (:body (:h1 "hello")
                 (:p "hello")))))

(defmethod call ((app test-app) request)
  (let ((*html-output* *standard-output*))
    (/hello)))

(defun start-test-server (&optional (handler (make-instance 'info.read-eval-print.httpd:default-handler)))
  (setf *test-server*
        (make-instance 'info.read-eval-print.httpd:server
                       :document-root *test-document-root*
                       :application 'test-app
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

(deftest test-get ()
  (iolib.sockets:with-open-socket (socket :remote-host *test-host*
                                          :remote-port *test-port*
                                          :external-format :utf-8)
    (send-line socket "GET /index.html HTTP/1.1")
    (send-line socket "Host: localhost")
    (send-line socket "")
    (is (string= "HTTP/1.1 200 OK" (receive-line socket))))
  (is (string= (path-content "/index.html")
               (drakma:http-request (url "/index.html")))))

(deftest test-post ()
  (multiple-value-bind (content code)
      (drakma:http-request (format nil "http://~a:~a/index.html" *test-host* *test-port*)
                           :method :post
                           :parameters ' (("ab" . "cd")
                                          ("ef" . "gh")))
    (declare (ignore content))
    (is (= 200 code)))
  (multiple-value-bind (content code)
      (drakma:http-request (format nil "http://~a:~a/index.html" *test-host* *test-port*)
                           :method :post
                           :content-length 11
                           :parameters ' (("ab" . "cd")
                                          ("ef" . "gh")))
    (declare (ignore content))
    (is (= 200 code))))

(deftest test-app-get ()
  (is (string= (with-output-to-string (*html-output*)
                 (/hello))
               (drakma:http-request (url "/hello")))))

(deftest test-app-get-http/1.0 ()
  (is (string= (with-output-to-string (*html-output*)
                 (/hello))
               (drakma:http-request (url "/hello")
                                    :protocol :http/1.0))))

(deftest test-ssl-server ()
  (let* ((port 4443)
         (server (make-instance 'info.read-eval-print.httpd:ssl-server
                                :document-root *test-document-root*
                                :port port
                                :application 'test-app
                                :ssl-cert (namestring (merge-pathnames "ssl/ssl.pem" *dir*))
                                :ssl-key (namestring (merge-pathnames "ssl/ssl.key" *dir*)))))
    (sb-thread:make-thread (lambda ()
                             (info.read-eval-print.httpd:start server))
                           :name "test ssl server")
    (sleep 0.1)
    (unwind-protect
         (progn
           (is (string= (with-output-to-string (*html-output*)
                          (/hello))
                        (drakma:http-request (format nil "https://localhost:~a/hello" port))))
           (is (string= (with-output-to-string (*html-output*)
                          (/hello))
                        (drakma:http-request (format nil "https://localhost:~a/hello" port)
                                             :protocol :http/1.0)))
           (multiple-value-bind (content code)
               (drakma:http-request (format nil "https://localhost:~a/index.html" port))
             (is (= 200 code))
             (is (string= (path-content "/index.html") content))))
      (setf (info.read-eval-print.httpd:quit-p server) t))))


(info.read-eval-print.httpd.test)
