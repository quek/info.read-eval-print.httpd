(asdf:defsystem :info.read-eval-print.httpd
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse")
               (:file "response")
               (:file "app-handler")
               (:file "httpd"))
  :depends-on (:iolib
               :cl-fad
               :anaphora
               :trivial-gray-streams
               :info.read-eval-print.series-ext))
