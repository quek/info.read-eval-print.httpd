(asdf:defsystem :info.read-eval-print.httpd
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "ffi")
               (:file "thread-local")
               (:file "parse")
               (:file "request")
               (:file "response")
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
               :info.read-eval-print.series-ext))
