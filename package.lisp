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

          #:authorization
          #:unauthorized
          #:redirect
          #:redirect-premanently

          #:call
          #:env
          #:params
          #:with-params

          #:*app-error-handler*
          #:default-app-error-handler
          #:ignore-app-error-handler))
