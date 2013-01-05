(in-package :info.read-eval-print.httpd)

(defvar *thread-local* nil "この変更はこのファイル内だけで使いましょう。
parse.lisp でも使っちゃった。")

(defstruct thread-local
  (server)
  (fd-hash (make-hash-table))
  (timers (iomux::make-priority-queue :key #'iomux::%timer-expire-time))
  (epoll-fd (isys:epoll-create 1))
  (buffer (make-octet-vector 4096))
  (dispatch-in-table (make-hash-table))
  (dispatch-out-table (make-hash-table))
  (dispatch-hup-table (make-hash-table)))


(defconstant epollet (ash 1 31))

(defun epoll-ctl (fd epfd op &rest events)
  (cffi:with-foreign-object (ev 'isys:epoll-event)
    (isys:bzero ev isys:size-of-epoll-event)
    (setf (cffi:foreign-slot-value ev 'isys:epoll-event 'isys:events)
          (apply #'logior events))
    (setf (cffi:foreign-slot-value
           (cffi:foreign-slot-value ev 'isys:epoll-event 'isys:data)
           'isys:epoll-data 'isys:fd)
          fd)
    (handler-case
        (isys:epoll-ctl epfd op fd ev)
      (isys:ebadf ()
        (warn "FD ~A is invalid, cannot monitor it." fd))
      (isys:eexist ()
        (warn "FD ~A is already monitored." fd)))))

(defparameter *response-header*
  (format nil "HTTP/1.0 200 OK~c~cLast-modified: Thu, 1 Jan 1970 00:00:00 GMT~c~c~c~c"
          #\cr #\lf #\cr #\lf #\cr #\lf))


(alexandria:define-constant +url-decode-table+
    (let ((array (make-array 255 :element-type 'fixnum
                                 :initial-element -1)))
      (let ((i (scan-range :from (char-code #\0) :upto (char-code #\9)))
            (n (scan-range)))
        (collect-ignore (setf (aref array i) n)))
      (let ((i (scan-range :from (char-code #\a) :upto (char-code #\f)))
            (n (scan-range)))
        (collect-ignore (setf (aref array i) (+ n 10))))
      (let ((i (scan-range :from (char-code #\A) :upto (char-code #\F)))
            (n (scan-range)))
        (collect-ignore (setf (aref array i) (+ n 10))))
      (setf (aref array (char-code #\+)) (char-code #\space))
      array)
  :test #'equalp)

(declaim (inline nurl-decode))
(defun nurl-decode (buffer start end)
  (declare (type (simple-array (unsigned-byte 8)) buffer))
  (loop with i = start
        for j from start
        do (when (= i end)
             (return-from nurl-decode
               (babel:octets-to-string buffer :encoding :utf-8
                                              :start start
                                              :end j)))
           (let ((code (aref buffer i)))
             (if (= code #.(char-code #\%))
                 (progn
                   (setf (aref buffer j)
                         (+ (* (aref +url-decode-table+ (aref buffer (1+ i))) 16)
                            (aref +url-decode-table+ (aref buffer (+ 2 i)))))
                   (incf i 3))
                 (progn (setf (aref buffer j) code)
                        (incf i))))))

(defgeneric start (server))
(defgeneric stop (server))
(defgeneric install-application (server application &key context-root))

(defgeneric handle-request (handler server client)
  (:method-combination or))

(defgeneric accept (server))
(defgeneric receive-request (fd))
(defgeneric send-response (fd))
(defgeneric close-connection (fd))

(defclass server ()
  ((document-root :initarg :document-root :initform "/usr/share/doc")
   (port :initarg :port :initform 1958)
   (bind-address :initarg :bind-address :initform "0.0.0.0")
   (listen-socket)
   (listen-fd)
   (number-of-threads :initarg :number-of-threads :initform 2)
   (handler :initarg :handler :initform (make-instance 'default-handler))
   (keep-alive-timeout :initarg :keep-alive-timer :initform 10)
   (quit :initform nil :accessor quit-p)
   (accept-threads :initform nil)))

(defclass ssl-server (server)
  ((ssl-cert :initarg :ssl-cert :initform "/etc/ssl/certs/ssl-cert-snakeoil.pem")
   (ssl-key :initarg :ssl-key :initform "/etc/ssl/private/ssl-cert-snakeoil.key")))

(defgeneric ssl-p (server)
  (:method (server)
    nil)
  (:method ((ssl-server ssl-server))
    t))

(defmethod keep-alive-p ((server server))
  (with-slots (keep-alive-timeout) server
    keep-alive-timeout))

(defgeneric make-client (server &rest args)
  (:method (server &rest args)
    (apply #'make-instance 'http-client :server server args))
  (:method ((server ssl-server) &rest args)
    (with-slots (ssl-cert ssl-key) server
      (apply #'make-instance 'https-client
             :server server
             :ssl-cert ssl-cert
             :ssl-key ssl-key
             args))))

(defmethod initialize-instance :after ((server server) &key application applications)
  (with-slots (document-root) server
    (setf document-root
          (string-right-trim '(#\/)
                             (if (pathnamep document-root)
                                 (directory-namestring document-root)
                                 document-root))))
  (when application
    (if (atom application)
        (install-application server application)
        (apply #'install-application server application)))
  (mapcan (lambda (x)
            (if (atom x)
                (install-application server x)
                (apply #'install-application server x)))
          applications))

(defmethod (setf quit-p) :after (value (server server))
  (when value
    (close (slot-value server 'listen-socket))
    (dolist (thread (slot-value server 'accept-threads))
      (sb-thread:terminate-thread thread))))


(defun set-keep-alive-timer (fd client)
  (let ((server (thread-local-server *thread-local*)))
   (with-slots (keep-alive-timer) client
     (when (and (keep-alive-p client) (not keep-alive-timer))
       (let ((timer (iomux::make-timer
                     (lambda ()
                       (print (list fd "close keep alive connection!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
                       (close-connection fd))
                     (slot-value server 'keep-alive-timeout)
                     :one-shot t)))
         (setf keep-alive-timer timer)
         (iomux::schedule-timer (thread-local-timers *thread-local*) timer))))))

(declaim (inline add-to-epoll-wait))
(defun add-to-epoll-wait (server pipe-read-fd pipe-write-fd)
  (cffi:with-foreign-object (buf :int) (sb-posix:read pipe-read-fd buf #.(cffi:foreign-type-size :int))
    (let ((fd (cffi:mem-aref buf :int 0)))
      (sif (gethash fd (thread-local-fd-hash *thread-local*))
           (if (keep-alive-p it)
               (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-mod isys:epollin isys:epollhup epollet)
               (close-connection fd))
           (progn
             (setf it (make-client server :fd fd :pipe-write-fd pipe-write-fd))
             (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-add isys:epollin isys:epollhup epollet))))))

(defun dispatch-epoll-event (fd event)
  (let ((event-mask (cffi:foreign-slot-value event 'isys:epoll-event 'isys:events)))
    (cond ((logtest event-mask isys:epollin)
           (funcall (gethash fd (thread-local-dispatch-in-table *thread-local*) 'receive-request)
                    fd))
          ((logtest event-mask isys:epollout)
           (funcall (gethash fd (thread-local-dispatch-out-table *thread-local*) 'send-response)
                    fd))
          ((logtest event-mask isys:epollhup)
           (funcall (gethash fd (thread-local-dispatch-hup-table *thread-local*) 'close-connection)
                    fd)))))

(declaim (inline handle-events))
(defgeneric handle-events (server events ready-fds pipe-read-fd pipe-write-fd))
(defmethod handle-events ((server server) events ready-fds pipe-read-fd pipe-write-fd)
  (loop for i below ready-fds
        for event = (cffi:mem-aref events 'isys:epoll-event i)
        for event-fd = (cffi:foreign-slot-value
                        (cffi:foreign-slot-value event 'isys:epoll-event 'isys:data)
                        'isys:epoll-data 'isys:fd)
        do (handler-case
               (if (= event-fd pipe-read-fd)
                   (add-to-epoll-wait server pipe-read-fd pipe-write-fd)
                   (dispatch-epoll-event event-fd event))
             #+nil
             (error (e)
               (warn "~a" e)
               (ignore-errors
                (funcall (gethash event-fd (thread-local-dispatch-hup-table *thread-local*)
                                  'close-connection)
                         event-fd)))))
  (iomux::expire-pending-timers (thread-local-timers *thread-local*) (isys:get-monotonic-time)))

(defun start-handler-thread (server pipe-read-fd pipe-write-fd)
  (let ((*thread-local* (make-thread-local :server server)))
    (epoll-ctl pipe-read-fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-add isys:epollin)
    (cffi:with-foreign-object (events 'isys:epoll-event iomux::+epoll-max-events+)
      (isys:bzero events (* iomux::+epoll-max-events+ isys:size-of-epoll-event))
      (loop for ready-fds = (handler-case (isys:epoll-wait (thread-local-epoll-fd *thread-local*)
                                                           events iomux::+epoll-max-events+ 500)
                              (isys:eintr ()
                                (warn "epoll-wait EINTR")
                                0))
            until (quit-p server)
            do (handle-events server events ready-fds pipe-read-fd pipe-write-fd)))))

(defun start-accept-thread (server)
  (multiple-value-bind (pipe-read-fd pipe-write-fd) (sb-posix:pipe)
    (sb-thread:make-thread #'start-handler-thread
                           :name (format nil "handler of ~a"
                                         (sb-thread:thread-name sb-thread:*current-thread*))
                           :arguments (list server pipe-read-fd pipe-write-fd))
    (cffi:with-foreign-object (buf :int)
      (loop with listen-fd = (slot-value server 'listen-fd)
            for client-fd = (iolib.sockets::with-sockaddr-storage-and-socklen (ss size)
                              (iolib.sockets::%accept listen-fd ss size))
            until (quit-p server)
            do (setf (isys:fd-nonblock client-fd) t)
               (setf (cffi:mem-aref buf :int 0) client-fd)
               (sys-write pipe-write-fd buf #.(cffi:foreign-type-size :int))))))

(defmethod start (server)
  (with-slots (port bind-address listen-socket listen-fd number-of-threads accept-threads) server
    (iolib.sockets:with-open-socket (%listen-socket :address-family :ipv4
                                                    :type :stream
                                                    :connect :passive
                                                    :local-host bind-address
                                                    :local-port port
                                                    :reuse-address t
                                                    :backlog iolib.sockets::+max-backlog-size+)
      (setf listen-socket %listen-socket)
      (setf listen-fd (iolib.streams:fd-of listen-socket))
      (setf (isys:fd-nonblock listen-fd) nil)
      (setf accept-threads (collect 'bag
                             (let ((thread-name (format nil "accepter ~d"
                                                        (1+ (scan-range :length number-of-threads)))))
                               (sb-thread:make-thread #'start-accept-thread
                                                      :name thread-name
                                                      :arguments (list server)))))
      (ignore-errors (collect-ignore (sb-thread:join-thread (scan accept-threads)))))))

(defmethod install-application ((server server) application &key (context-root ""))
  (with-slots (handler) server
    (install-application handler application :context-root context-root)))

(defmethod normalize-request (server client buffer position fd)
  (with-slots (document-root) server
    (setf (env client :document-root) document-root)
    (let* ((request-uri (env client :request-uri))
           (? (position #\? request-uri :start 1)))
      (setf (env client :script-name) "")
      (setf (env client :path-info) (subseq request-uri 0 ?))
      (setf (env client :query-string) (if ? (subseq request-uri (1+ ?)) "")))
    (setf (slot-value client 'fd) fd)
    #+nil
    (iterate (((k v) (scan-hash (slot-value request 'env))))
      (format t "~a: ~a~%" k v))
    client))

(defmethod parse-request (server fd buffer buffer-size client)
  (with-slots (remain-request-buffer) client
    (prog ()
     :start
       (multiple-value-bind (ok position) (start-parse-request buffer 0 buffer-size client)
         (if ok
             (progn
               (normalize-request server client buffer position fd)
               (set-keep-alive-timer fd client)
               (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))
             ;; TODO buffer の上限。
             (let* ((remain-size (- buffer-size position))
                    (new-buffer-size (* 2 (length buffer)))
                    (new-buffer (make-array new-buffer-size :element-type '(unsigned-byte 8))))
               (replace new-buffer buffer :start2 position :end2 buffer-size)
               #+nil
               (iterate ((si (scan-range :from position :below buffer-size))
                         (di (scan-range)))
                 (setf (aref new-buffer di) (aref buffer si)))
               (setf (thread-local-buffer *thread-local*) new-buffer)
               (let ((read-size
                       (handler-case (read-sequence new-buffer client
                                                    :start remain-size
                                                    :end new-buffer-size)
                         (io-block-error ()
                           (setf remain-request-buffer
                                 (subseq (thread-local-buffer *thread-local*) 0 remain-size))
                           (return)))))
                 (setf buffer new-buffer)
                 (setf buffer-size (+ read-size remain-size))
                 (go :start))))))))


(defmethod receive-request (fd)
  (let ((server (thread-local-server *thread-local*))
        (client (gethash fd (thread-local-fd-hash *thread-local*))))
    (with-slots (remain-request-buffer) client
      (let* ((start (let* ((buf remain-request-buffer)
                           (len (length buf)))
                      (iterate ((i (scan-range :length len)))
                        (setf (aref (thread-local-buffer *thread-local*) i) (aref buf i)))
                      (setf remain-request-buffer nil)
                      len))
             (buffer (thread-local-buffer *thread-local*))
             (read-size (read-sequence buffer client :start start :end (length buffer))))
        (if (plusp read-size)
            (parse-request server fd (thread-local-buffer *thread-local*) (+ start read-size) client)
            (close-connection fd))))))

(defun reset-client-fd-for-keep-alive (client fd)
  (reset-client client)
  (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-mod isys:epollin isys:epollhup epollet))

(defmethod send-response (fd)
  (catch 'send-response
    (let ((server (thread-local-server *thread-local*)))
      (with-slots (document-root handler) server
        (let ((client (gethash fd (thread-local-fd-hash *thread-local*))))
          (case (handle-request handler server client)
            (:detach
             (detach-fd fd))
            (t (if (keep-alive-p client)
                   (reset-client-fd-for-keep-alive client fd)
                   (close-connection fd)))))))))

(defun detach-fd (fd)
  (let ((client (gethash fd (thread-local-fd-hash *thread-local*))))
    (awhen (and client (slot-value client 'keep-alive-timer))
      (iomux::unschedule-timer (thread-local-timers *thread-local*) it))
    (remhash fd (thread-local-fd-hash *thread-local*))
    (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-del)))


(defmacro with-cork ((fd) &body body)
  "sendfile の前のレスポンスヘッダを小分けで送らないために TCP_CROK"
  (alexandria:once-only (fd)
    `(progn
       (iolib.sockets::set-socket-option-int ,fd iolib.sockets::ipproto-tcp
                                             iolib.sockets::tcp-cork 1)
       ,@body
       (iolib.sockets::set-socket-option-int ,fd iolib.sockets::ipproto-tcp
                                             iolib.sockets::tcp-cork 0))))

(defgeneric sendfile (client fd path))
(defmethod sendfile ((client http-client) fd path)
  (let* ((file-fd (handler-case (isys::open path isys:o-rdonly)
                    (isys:enoent ()
                      (warn "404 not found ~a" path)
                      (return-from sendfile nil))))
         (st (isys:fstat file-fd))
         (last-modified (rfc-2822-posix (isys:stat-mtime st)))
         (content-length (isys:stat-size st)))
    (delete-header client "Transfer-Encoding")
    (with-cork (fd)
      (format (raw-stream client) "~
HTTP/1.1 200 OK~a~
Content-Type: ~a~a~
Content-Length: ~d~a~
Date: ~a~a~
Last-modified: ~a~a~
~a"
              +crlf+
              (path-mime-type path) +crlf+
              content-length +crlf+
              (rfc-2822-now) +crlf+
              last-modified +crlf+
              +crlf+)
      (let ((offset (cffi:foreign-alloc :long :initial-element 0)))
        (sendfile-loop fd file-fd offset content-length)))
    t))

(defmethod sendfile ((client https-client) fd path)
  (let ((buffer (make-octet-vector #1=4096))
        (src (handler-case (open path :element-type '(unsigned-byte 8))
               (error ()
                 (warn "404 not found ~a" path)
                 (return-from sendfile nil)))))
    (ssl-sendfile-loop client src buffer #1#)))

(defun sendfile-loop (dist-fd src-fd offset length)
  (loop for done = (handler-case (%sendfile dist-fd src-fd offset length)
                     (iolib.syscalls:ewouldblock ()
                       (setf (gethash dist-fd (thread-local-dispatch-out-table *thread-local*))
                             (lambda (dist-fd)
                               (let ((client (gethash dist-fd (thread-local-fd-hash *thread-local*))))
                                 (sendfile-loop dist-fd src-fd offset length)
                                 (remhash dist-fd (thread-local-dispatch-out-table *thread-local*))
                                 (if (keep-alive-p client)
                                     (reset-client-fd-for-keep-alive client dist-fd)
                                     (close-connection dist-fd)))))
                       (throw 'send-response nil))
                     (error ()
                       (loop-finish)))
        until (zerop (decf length done)))
  (cffi-sys:foreign-free offset)
  (isys:close src-fd)
  t)

(defun ssl-sendfile-loop (client src buffer bufsiz)
  (loop  for len = (read-sequence buffer src :end bufsiz)
         until (not (plusp len))
         do (handler-case (write-sequence buffer client :end len)
              (io-block-error ()
                (with-slots (fd) client
                  (setf (gethash fd (thread-local-dispatch-out-table *thread-local*))
                        (lambda (fd)
                          (ssl-sendfile-loop client src buffer bufsiz)
                          (remhash fd (thread-local-dispatch-out-table *thread-local*))
                          (if (keep-alive-p client)
                              (reset-client-fd-for-keep-alive client fd)
                              (close-connection fd))))
                  (throw 'send-response nil)))
              (error ()
                (loop-finish))))
  (force-output client)
  (close src)
  t)

(defmethod close-connection (fd)
  (detach-fd fd)
  (ignore-errors (isys:close fd)))


(defclass sendfile-handler ()
  ())

(defmethod handle-request or ((handler sendfile-handler) server client)
  (with-slots (fd) client
    (let* ((path (concatenate 'string
                              (env client :document-root)
                              (env client :path-info))))
      (when (and (fad:file-exists-p path)
                 (not (fad:directory-exists-p path)))
        (sendfile client fd path)))))


(defclass 404-handler ()
  ())

(defmethod handle-request or ((handler 404-handler) server client)
  (setf (response-status-of client) 404)
  (format client  "<html>
<head><title>not found</title></head>
<body>Not Found. ~a</body></html>" (h (env client :path-info)) )
  (force-output client)
  t)


(defclass default-handler (sendfile-handler app-handler 404-handler)
  ())

#|
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server
                                                 :application '(env-dump-app :context-root "/env")))
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server
                                                 :document-root "/tmp"))
ab -n 10000 -c 10 'http://localhost:1958/sbcl-doc/html/index.html'

(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:ssl-server
                                                 :ssl-cert "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.pem"
                                                 :ssl-key "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.key"
                                                 :application '(env-dump-app :context-root "/env")))

|#
