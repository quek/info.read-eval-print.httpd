(in-package :info.read-eval-print.httpd)

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

(isys:defsyscall (%sendfile "sendfile") :int
  (out-fd :int)
  (in-fd :int)
  (offset (:pointer isys::off-t))
  (count isys::size-t))

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

(defconstant +bufsiz+ 4096)

(defgeneric start (server))
(defgeneric stop (server))

(defgeneric handle-request (handler server fd env)
  (:method-combination or))
(defgeneric call (handler env))

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
   (number-of-threads :initarg :number-of-threads :initform 2)
   (handler :initarg :handler :initform (make-instance 'default-handler))))

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
        (*buffer* (make-array +bufsiz+ :element-type '(unsigned-byte 8))))
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

(defun make-env (buffer)
  (let ((env (make-hash-table :test 'equal))
        (start 0)
        (end (position #.(char-code #\space) buffer :start 3)))
    (setf (gethash "REQUEST_METHOD" env)
          (sb-ext:octets-to-string buffer :start start :end end))
    (setf start (incf end))
    (setf end (position #.(char-code #\space) buffer :start (incf end)))
    (let ((request-uri (sb-ext:octets-to-string buffer :start start :end end)))
      (setf (gethash "REQUEST_URI" env) request-uri)
      (let ((? (position #.(char-code #\?) buffer :start start :end end)))
        (setf (gethash "QUERY_STRING" env)
              (if ?
                  (sb-ext:octets-to-string buffer :start (1+ ?) :end end)
                  ""))
        (setf (gethash "PATH_INFO" env) (nurl-decode buffer start (or ? end)))))
    (setf start (incf end))
    (setf end (position #.(char-code #\cr) buffer :start start))
    (setf (gethash "SERVER_PROTOCOL" env) (sb-ext:octets-to-string buffer :start start :end end))
    #+nil
    (print (multiple-value-bind (k v) (scan-hash env)
             (collect-alist k v)))
    env))

(defmethod parse-request (server fd buffer)
  (with-slots (document-root) server
    (let ((env (make-env buffer)))
      (setf (gethash fd *fd-hash*) env)
      (setf (gethash "DOCUMENT_ROOT" env) document-root)
      (setf (gethash :fd env) fd)))
  (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))


(defmethod receive-request (server fd)
  (let ((read-size
          (cffi-sys:with-pointer-to-vector-data (pointer *buffer*)
            (iolib.syscalls:read fd pointer +bufsiz+))))
    (if (plusp read-size)
        (progn
          (parse-request server fd *buffer*)
          (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))
        (close-connection server fd))))

(defvar *env*)

(defmethod send-response (server fd)
  (with-slots (document-root handler) server
    (let ((*env* (gethash fd *fd-hash*)))
      (when *env*
        (handle-request handler server fd *env*)))
    (close-connection server fd)))

(defun status-message (status)
  (case status
    (200 "OK")
    (404 "Not Found")
    (t "que")))

(defun write-response (string)
  (let ((fd (gethash :fd *env*)))
    (cffi:with-foreign-string ((s length) string :encoding :utf-8 :null-terminated-p nil)
      (isys:write fd s length))))

(defmacro with-cork ((fd) &body body)
  "sendfile の前のレスポンスヘッダを小分けで送らないために TCP_CROK"
  (alexandria:once-only (fd)
    `(progn
       (iolib.sockets::set-socket-option-int ,fd iolib.sockets::ipproto-tcp
                                             iolib.sockets::tcp-cork 1)
       ,@body
       (iolib.sockets::set-socket-option-int ,fd iolib.sockets::ipproto-tcp
                                             iolib.sockets::tcp-cork 0))))

(defun sendfile (fd path)
  (let* ((file-fd (handler-case (isys::open path isys:o-rdonly)
                    (isys:enoent ()
                      (warn "404 not found ~a" path)
                      (return-from sendfile nil))))
         (st (isys:fstat file-fd)))
    (with-cork (fd)
      (cffi:with-foreign-string ((s length)
                                 (format nil "HTTP/1.0 200 OK~aLast-modified: Thu, 1 Jan 1970 00:00:00 GMT~a~a"
                                         +crlf+ +crlf+ +crlf+))
            (isys:write fd s length))
      (%sendfile fd file-fd (cffi-sys:null-pointer) (isys:stat-size st)))
    (isys:close file-fd)
    t))

(defun %send-response (status headers body)
  (let ((fd (gethash :fd *env*)))
    (aif (cdr (assoc "X-Sendfile" headers :test #'equal))
         (unless (sendfile fd it)
           (error "X-Sendfile not found"))
         (with-cork (fd)
           (write-response (format nil "HTTP/1.0 ~d ~a~a" status (status-message status) +crlf+))
           (write-response (collect-append
                            'string
                            (multiple-value-bind (k v) (scan-alist headers #'equal)
                              (format nil "~a: ~a~a" k v +crlf+))))
           (write-response +crlf+)
           (collect-ignore (write-response (scan body)))
           t))))


(defmethod close-connection (server fd)
  (remhash fd *fd-hash*)
  (epoll-ctl fd *epoll-fd* isys:epoll-ctl-del)
  (isys:close fd))

(defmethod response-404 (handler env)
  (values ))


(defclass sendfile-handler ()
  ())

(defmethod handle-request or ((handler sendfile-handler) server fd env)
  (let* ((path (concatenate 'string
                             (gethash "DOCUMENT_ROOT" env)
                             (gethash "PATH_INFO" env))))
    (when (and (fad:file-exists-p path)
               (not (fad:directory-exists-p path)))
      (sendfile fd path))))

(defclass 404-handler ()
  ())

(defun h (s)
  (with-output-to-string (out)
    (iterate ((c (scan 'string s)))
      (princ (case c
               (#\& "&amp;")
               (#\< "&lt;")
               (#\> "&gt;")
               (#\" "&quot;")
               (t c))
             out))))

(defmethod handle-request or ((handler 404-handler) server fd env)
  (%send-response 404 `(("Content-type" . "text/html"))
                  (list "<html>
<head><title>not found</title></head>
<body>Not Found. " (h (gethash "PATH_INFO" env)) "</body></html>")))


(defclass app-handler ()
  ((app :initarg :app :initform (make-instance 'env-dum-app))))

(defmethod handle-request or ((handler app-handler) server fd env)
  (multiple-value-call #'%send-response (call handler *env*)))


(defclass env-dump-app ()
  ())

(defmethod call ((aap env-dump-app) env)
  (values 200
          `(("Content-Type" . "text/html; charset=UTF-8"))
           (list "<ul>"
                 (collect-append 'string
                                  (multiple-value-bind (k v) (scan-hash  env)
                                    (format nil "<li>~a . ~a</li>" k v)))
                 "</ul>")))


(defclass cgi-handler ()
  ())

(defmethod handle-request or ((handler cgi-handler) server fd env)
  (let ((path (concatenate 'string (gethash "DOCUMENT_ROOT" env)
                           (gethash "PATH_INFO" env))))
    (when (alexandria:ends-with-subseq ".cgi" path :test #'equal)
      (let ((body (with-output-to-string (body)
                    (sb-ext:run-program
                     "/bin/sh" (list path)
                     :output body
                     :environment (collect 'bag
                                    (multiple-value-bind (k v) (scan-hash env)
                                      (format nil "~a=~a" k v)))))))
        (write-response (format nil "HTTP/1.0 200 OK~a" +crlf+))
        (write-response body)
        t))))

(defclass default-handler (cgi-handler sendfile-handler 404-handler)
  ())

#|
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server))
(info.read-eval-print.httpd:start (make-instance 'info.read-eval-print.httpd:server
                                                 :document-root "/tmp"))
ab -n 10000 -c 10 'http://localhost:1958/sbcl-doc/html/index.html'
|#
