(asdf:defsystem :info.read-eval-print.httpd
  :serial t
  :components ((:file "package")
               (:file "httpd"))
  :depends-on (:iolib
               :cl-fad
               :anaphora
               :info.read-eval-print.series-ext))
