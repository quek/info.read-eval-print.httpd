(asdf:defsystem :info.read-eval-print.httpd
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse")
               (:file "request")
               (:file "response")
               (:file "app-handler")
               (:file "cgi-handler")
               (:file "httpd"))
  :depends-on (:iolib
               :cl-fad
               :anaphora
               :trivial-gray-streams
               :sb-concurrency
               :info.read-eval-print.series-ext))
