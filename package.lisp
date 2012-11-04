;;;; package.lisp

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.httpd
 (:use :cl :anaphora)
 (:export #:start
          #:server
          #:quit-p

          #:default-handler
          #:cgi-handler
          #:sendfile-handler
          #:404-handler
          #:app-handler

          #:call
          #:env
          #:params))
