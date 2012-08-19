(asdf:defsystem :info.read-eval-print.httpd
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse")
               (:file "httpd"))
  :depends-on (:iolib
               :cl-fad
               :anaphora
               :simple-date-time
               :info.read-eval-print.series-ext))
