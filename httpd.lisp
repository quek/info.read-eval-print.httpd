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

(defgeneric handle-request (handler server request)
  (:method-combination or))

(defgeneric accept (server))
(defgeneric receive-request (fd))
(defgeneric send-response (fd))
(defgeneric close-connection (fd-or-request))

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

(defgeneric make-request (server &rest args)
  (:method (server &rest args)
    (apply #'make-instance 'request :server server args))
  (:method ((server ssl-server) &rest args)
    (with-slots (ssl-cert ssl-key) server
      (apply #'make-instance 'ssl-request
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


(defun set-keep-alive-timer (fd request)
  (let ((server (thread-local-server *thread-local*)))
   (with-slots (keep-alive-timer) request
     (when (and (keep-alive-p server request) (not keep-alive-timer))
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
           (if (keep-alive-p (thread-local-server *thread-local*) it)
               (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-mod isys:epollin isys:epollhup epollet)
               (close-connection fd))
           (progn
             (setf it (make-request server :fd fd :pipe-write-fd pipe-write-fd))
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

(defmethod normalize-request (server request buffer position fd)
  (with-slots (document-root) server
    (setf (env request :document-root) document-root)
    (let* ((request-uri (env request :request-uri))
           (? (position #\? request-uri :start 1)))
      (setf (env request :script-name) "")
      (setf (env request :path-info) (subseq request-uri 0 ?))
      (setf (env request :query-string) (if ? (subseq request-uri (1+ ?)) "")))
    (setf (slot-value request 'fd) fd)
    #+nil
    (iterate (((k v) (scan-hash (slot-value request 'env))))
      (format t "~a: ~a~%" k v))
    request))

(defmethod parse-request (server fd buffer buffer-size request)
  (with-slots (remain-request-buffer) request
    (prog ()
     :start
       (multiple-value-bind (ok position) (start-parse-request buffer 0 buffer-size request)
         (if ok
             (progn
               (normalize-request server request buffer position fd)
               (set-keep-alive-timer fd request)
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
               (let ((read-size (receive-from request new-buffer remain-size new-buffer-size)))
                 (unless read-size
                   (setf remain-request-buffer (subseq (thread-local-buffer *thread-local*) 0 remain-size))
                   (return))
                 (setf buffer new-buffer)
                 (setf buffer-size (+ read-size remain-size))
                 (go :start))))))))


(defmethod receive-request (fd)
  (let ((server (thread-local-server *thread-local*))
        (request (gethash fd (thread-local-fd-hash *thread-local*))))
    (with-slots (remain-request-buffer) request
      (let* ((start (let* ((buf remain-request-buffer)
                           (len (length buf)))
                      (iterate ((i (scan-range :length len)))
                        (setf (aref (thread-local-buffer *thread-local*) i) (aref buf i)))
                      (setf remain-request-buffer nil)
                      len))
             (buffer (thread-local-buffer *thread-local*))
             (read-size (receive-from request buffer start (length buffer))))
        (if (plusp read-size)
            (parse-request server fd (thread-local-buffer *thread-local*) (+ start read-size) request)
            (close-connection fd))))))

(defun reset-client-fd-for-keep-alive (request fd)
  (reset-request request)
  (epoll-ctl fd (thread-local-epoll-fd *thread-local*) isys:epoll-ctl-mod isys:epollin isys:epollhup epollet))

(defmethod send-response (fd)
  (catch 'send-response
    (let ((server (thread-local-server *thread-local*)))
      (with-slots (document-root handler) server
        (let ((request (gethash fd (thread-local-fd-hash *thread-local*))))
          (case (handle-request handler server request)
            (:detach
             (detach-fd fd))
            (t (if (keep-alive-p server request)
                   (reset-client-fd-for-keep-alive request fd)
                   (close-connection fd)))))))))

(defun detach-fd (fd)
  (let ((request (gethash fd (thread-local-fd-hash *thread-local*))))
    (awhen (and request (slot-value request 'keep-alive-timer))
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

(defgeneric sendfile (request fd path))
(defmethod sendfile ((request request) fd path)
  (let* ((file-fd (handler-case (isys::open path isys:o-rdonly)
                    (isys:enoent ()
                      (warn "404 not found ~a" path)
                      (return-from sendfile nil))))
         (st (isys:fstat file-fd))
         (last-modified (rfc-2822-posix (isys:stat-mtime st)))
         (content-length (isys:stat-size st))
         (response (response-of request)))
    (with-cork (fd)
      (write-to-response response
                         (format nil "~
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
                         nil)
      (let ((offset (cffi:foreign-alloc :long :initial-element 0)))
        (sendfile-loop fd file-fd offset content-length)))
    t))

(defmethod sendfile ((request ssl-request) fd path)
  (let ((response (response-of request)))
    (let ((buffer (make-octet-vector #1=4096))
          (src (handler-case (open path :element-type '(unsigned-byte 8))
                 (error ()
                   (warn "404 not found ~a" path)
                   (return-from sendfile nil)))))
      (ssl-sendfile-loop response src buffer #1#))))

(defun sendfile-loop (dist-fd src-fd offset length)
  (loop for done = (handler-case (%sendfile dist-fd src-fd offset length)
                     (iolib.syscalls:ewouldblock ()
                       (setf (gethash dist-fd (thread-local-dispatch-out-table *thread-local*))
                             (lambda (dist-fd)
                               (let ((request (gethash dist-fd (thread-local-fd-hash *thread-local*))))
                                 (sendfile-loop dist-fd src-fd offset length)
                                 (remhash dist-fd (thread-local-dispatch-out-table *thread-local*))
                                 (if (keep-alive-p (thread-local-server *thread-local*) request)
                                     (reset-client-fd-for-keep-alive request dist-fd)
                                     (close-connection dist-fd)))))
                       (throw 'send-response nil))
                     (error ()
                       (loop-finish)))
        until (zerop (decf length done)))
  (cffi-sys:foreign-free offset)
  (isys:close src-fd)
  t)

(defun ssl-sendfile-loop (dst src buffer bufsiz)
  (loop  for len = (read-sequence buffer src :end bufsiz)
         until (not (plusp len))
         do (handler-case (write-sequence buffer dst :end len)
              (iolib.syscalls:ewouldblock ()
                (with-slots (fd) dst
                  (setf (gethash fd (thread-local-dispatch-out-table *thread-local*))
                        (lambda (fd)
                          (let ((request (gethash fd (thread-local-fd-hash *thread-local*))))
                            (ssl-sendfile-loop dst src buffer bufsiz)
                            (remhash fd (thread-local-dispatch-out-table *thread-local*))
                            (if (keep-alive-p (thread-local-server *thread-local*) request)
                                (reset-client-fd-for-keep-alive request fd)
                                (close-connection fd)))))
                  (throw 'send-response nil)))
              (error ()
                (loop-finish))))
  (force-output dst)
  (close src)
  t)

(defmethod close-connection (fd)
  (detach-fd fd)
  (ignore-errors (isys:close fd)))


(defclass sendfile-handler ()
  ())

(defmethod handle-request or ((handler sendfile-handler) server request)
  (with-slots (fd) request
    (let* ((path (concatenate 'string
                              (env request :document-root)
                              (env request :path-info))))
      (when (and (fad:file-exists-p path)
                 (not (fad:directory-exists-p path)))
        (sendfile request fd path)))))


(defclass 404-handler ()
  ())

(defmethod handle-request or ((handler 404-handler) server request)
  (let ((response (make-response-stream request)))
    (setf (response-status-of response) 404)
    (format response  "<html>
<head><title>not found</title></head>
<body>Not Found. ~a</body></html>" (h (env request :path-info)) )
    (force-output response)
    t))


(defclass default-handler (sendfile-handler app-handler 404-handler)
  ())

#|
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server))
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server
                                                 :document-root "/tmp"))
ab -n 10000 -c 10 'http://localhost:1958/sbcl-doc/html/index.html'

(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:ssl-server
                                                 :ssl-cert "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.pem"
                                                 :ssl-key "/home/ancient/letter/lisp/craft/info.read-eval-print.httpd/ssl/ssl.key"))

|#
