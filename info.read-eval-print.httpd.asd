(asdf:defsystem :info.read-eval-print.httpd
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "ffi")
               (:file "parse")
               (:file "client")
               (:file "app-handler")
               (:file "cgi-handler")
               (:file "httpd"))
  :depends-on (:iolib
               :cl-fad
               :anaphora
               :cl-base64
               :trivial-gray-streams
               :sb-concurrency
               :percent-encoding
               :cl+ssl
               :info.read-eval-print.series-ext))
