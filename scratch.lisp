(ql:quickload :iolib)
(ql:quickload :drakma)

(iolib.sockets:with-open-socket (listen-socket :address-family :ipv4
                                               :type :stream
                                               :connect :passive
                                               :local-host "0.0.0.0"
                                               :local-port 2222
                                               :reuse-address t
                                               :backlog iolib.sockets::+max-backlog-size+)
  (iolib.sockets:with-accept-connection (socket listen-socket)
    (let ((ssl (cl+ssl:make-ssl-server-stream
                (iolib.streams:fd-of socket)
                :external-format :utf-8
                :certificate
                "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.pem"
                :key
                "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.key")))
      (print (read-line ssl))
      (format ssl "HTTP/1.1 200 OK
Content-Type: text/plain

hello")
      (force-output ssl))))

(print (drakma:http-request "https://localhost:2222"))



(ql:quickload :usocket)
(ql:quickload :cl+ssl)
(ql:quickload :drakma)

(usocket:with-socket-listener (server "localhost" 2223)
  (usocket:with-connected-socket (s (usocket:socket-accept server))
    (let ((socket (cl+ssl:make-ssl-server-stream
                   (usocket:socket-stream s)
                   :external-format :utf-8
                   :certificate
                   "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.pem"
                   :key
                   "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.key")))
      (print (read-line socket))
      (format socket "HTTP/1.1 200 OK
Content-Type: text/plain

hello")
      (force-output socket))))

(print (drakma:http-request "https://localhost:2223"))
