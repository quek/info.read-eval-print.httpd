(in-package ::info.read-eval-print.httpd)

(defconstant epollet (ash 1 31))

(alexandria:define-constant +crlf+ (format nil "~c~c" #\cr #\lf) :test #'equalp)

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

(isys:defsyscall (sendfile "sendfile") :int
  (out-fd :int)
  (in-fd :int)
  (offset (:pointer isys::off-t))
  (count isys::size-t))

(defun response-file (server fd path)
  (let* ((file-fd (handler-case (isys::open path isys:o-rdonly)
                    (isys:enoent ()
                      (warn "404 not found ~a" path)
                      (response-404 server fd)
                      (return-from response-file))))
         (st (isys:fstat file-fd)))
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp
                                          iolib.sockets::tcp-cork 1)
    (cffi:with-foreign-string (s *response-header*)
      (isys:write fd s (length *response-header*)))
    (sendfile fd file-fd (cffi-sys:null-pointer) (isys:stat-size st))
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp
                                          iolib.sockets::tcp-cork 0)
    (isys:close file-fd)))

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

(defconstant +bufsiz+ 4096)

(defgeneric start (server))
(defgeneric stop (server))

(defgeneric accept (server))
(defgeneric receive-request (server fd))
(defgeneric send-response (server fd))
(defgeneric close-connection (server fd))

(defclass server ()
  ((document-root :initarg :document-root :initform "/usr/share/doc")
   (port :initarg :port :initform 1958)
   (bind-address :initarg :bind-address :initform "0.0.0.0")
   (listen-socket)
   (listen-fd)
   (number-of-threads :initarg :number-of-threads :initform 2)))

(defvar *server*)
(defvar *fd-hash*)
(defvar *epoll-fd*)
(defvar *buffer*)

(defun add-to-wait (pipe-fd)
  (cffi:with-foreign-object (buf :int)
    (sb-posix:read pipe-fd buf #.(cffi:foreign-type-size :int))
    (let ((fd (cffi:mem-aref buf :int 0)))
      (epoll-ctl fd *epoll-fd* isys:epoll-ctl-add isys:epollin isys:epollhup epollet))))

(defun dispatch-epoll-event (fd event)
  (let ((event-mask (cffi:foreign-slot-value event 'isys:epoll-event 'isys:events)))
    (cond ((plusp (logand event-mask isys:epollin) )
           (receive-request *server* fd))
          ((plusp (logand event-mask isys:epollout))
           (send-response *server* fd))
          ((plusp (logand event-mask isys:epollhup))
           (close-connection *server* fd)))))

(defun start-handler-thread (server fd-hash epoll-fd pipe-fd)
  (let ((*server* server)
        (*fd-hash* fd-hash)
        (*epoll-fd* epoll-fd)
        (*buffer* (cffi:foreign-alloc :uchar :count +bufsiz+)))
    (cffi:with-foreign-object (events 'isys:epoll-event iomux::+epoll-max-events+)
      (isys:bzero events (* iomux::+epoll-max-events+ isys:size-of-epoll-event))
      (loop for ready-fds = (handler-case (isys:epoll-wait *epoll-fd* events iomux::+epoll-max-events+ 10000)
                              (isys:eintr ()
                                (warn "epoll-wait EINTR")
                                0))
            do (loop for i below ready-fds
                     for event = (cffi:mem-aref events 'isys:epoll-event i)
                     for event-fd = (cffi:foreign-slot-value
                                     (cffi:foreign-slot-value event 'isys:epoll-event 'isys:data)
                                     'isys:epoll-data 'isys:fd)
                     if (= event-fd pipe-fd)
                       do (add-to-wait pipe-fd)
                     else
                       do (dispatch-epoll-event event-fd event))))))

(defun start-accept-thread (server)
  (let ((*server* server)
        (*fd-hash* (make-hash-table))
        (*epoll-fd* (isys:epoll-create 1)))
    (multiple-value-bind (pipe-read pipe-write) (sb-posix:pipe)
      (epoll-ctl pipe-read *epoll-fd* isys:epoll-ctl-add isys:epollin)
      (sb-thread:make-thread #'start-handler-thread
                             :name (format nil "handler of ~a"
                                           (sb-thread:thread-name sb-thread:*current-thread*))
                             :arguments (list *server* *fd-hash* *epoll-fd* pipe-read))
      (cffi:with-foreign-object (buf :int)
        (loop with listen-fd = (slot-value server 'listen-fd)
              for client-fd = (iolib.sockets::with-sockaddr-storage-and-socklen (ss size)
                                (iolib.sockets::%accept listen-fd ss size))
              do (setf (cffi:mem-aref buf :int 0) client-fd)
                 (sb-posix:write pipe-write buf #.(cffi:foreign-type-size :int)))))))

(defmethod start (server)
  (with-slots (port bind-address listen-socket listen-fd number-of-threads) server
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
      (let ((threads (collect 'bag
                       (let ((thread-name (format nil "accepter ~d"
                                                  (1+ (scan-range :length number-of-threads)))))
                         (sb-thread:make-thread #'start-accept-thread
                                                :name thread-name
                                                :arguments (list server))))))
        (collect-ignore (sb-thread:join-thread (scan 'list threads)))))))

(defclass request ()
  ((method :initarg :method)
   (path :initarg :path)
   (query-string :initarg :query-string)))

(defun make-request (request-string)
  (let ((start 0)
        (end (position #\space request-string :start 3))
        method path query-string)
    (setf method (subseq request-string start end))
    (setf start (incf end))
    (setf end (position #\space request-string :start end))
    (setf path (subseq request-string start end))
    (let ((? (position #\? path)))
      (when ?
        (setf query-string (subseq path (1+ ?)))
        (setf path (subseq path 0 ?))))
    (make-instance 'request
                   :method method
                   :path path
                   :query-string query-string)))

(defmethod read-request (server fd request-string)
  (setf (gethash fd *fd-hash*)
        (make-request request-string))
  (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))


(defmethod receive-request (server fd)
  (let ((read-size (iolib.syscalls:read fd *buffer* +bufsiz+)))
    (if (plusp read-size)
        (progn
          (read-request server fd (cffi:foreign-string-to-lisp *buffer* :count read-size))
          (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))
        (close-connection server fd))))

(defmethod send-response (server fd)
  (print (list :send-respones sb-thread:*current-thread*))
  (with-slots (document-root) server
    (let ((request (gethash fd *fd-hash*)))
      (when request
        (with-slots (path) request
          (let ((full-path (concatenate 'string document-root path)))
            (if (and (fad:file-exists-p full-path)
                     (not (fad:directory-exists-p full-path)))
                (response-file server fd full-path)
                (response-404 server fd))))))
    (close-connection server fd)))

(defmethod close-connection (server fd)
  (remhash fd *fd-hash*)
  (epoll-ctl fd *epoll-fd* isys:epoll-ctl-del)
  (isys:close fd))

(defmethod response-404 (server fd)
  (let ((res (format nil "HTTP/1.0 404 Not Found~c~c~c~cNot Found"
                     #\cr #\lf #\cr #\lf)))
    (cffi:with-foreign-string (s res)
      (isys:write fd s (length res)))))

#|
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server))
|#
