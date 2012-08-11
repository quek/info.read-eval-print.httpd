# A tiny Web server.

    (info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server))

    ab -n 10000 -c 10 'http://localhost:1958/sbcl-doc/html/index.html'
