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
   (number-of-threads :initarg :number-of-threads :initform 1)
   (default-buffer-size :initarg :default-buffer-size :initform 10)
   (handler :initarg :handler :initform (make-instance 'default-handler))
   (keep-alive-timeout :initarg :keep-alive-timer :initform 10)))

(defvar *server*)
(defvar *fd-hash*)
(defvar *timers*)
(defvar *epoll-fd*)
(defvar *buffer*)

(defun set-keep-alive-timer (fd env)
  (when (and (keep-alive-p env) (not (gethash :keep-alive-timer env)))
    (let ((timer (iomux::make-timer
                  (lambda ()
                    (print (list fd "close keep alive connection!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
                    (close-connection *server* fd))
                  (slot-value *server* 'keep-alive-timeout)
                  :one-shot t)))
      (setf (gethash :keep-alive-timer env) timer)
      (iomux::schedule-timer *timers* timer))))

(defun add-to-wait (pipe-fd)
  (cffi:with-foreign-object (buf :int) (sb-posix:read pipe-fd buf #.(cffi:foreign-type-size :int))
    (let ((fd (cffi:mem-aref buf :int 0))
          (env (make-hash-table :test #'equal)))
      (setf (gethash fd *fd-hash*) env)
      (epoll-ctl fd *epoll-fd* isys:epoll-ctl-add isys:epollin isys:epollhup epollet))))

(defun dispatch-epoll-event (fd event)
  (let ((event-mask (cffi:foreign-slot-value event 'isys:epoll-event 'isys:events)))
    (cond ((plusp (logand event-mask isys:epollin) )
           (receive-request *server* fd))
          ((plusp (logand event-mask isys:epollout))
           (send-response *server* fd))
          ((plusp (logand event-mask isys:epollhup))
           (close-connection *server* fd)))))

;; (declaim (inline handle-events))
(defun handle-events (events ready-fds pipe-fd)
  (loop for i below ready-fds
        for event = (cffi:mem-aref events 'isys:epoll-event i)
        for event-fd = (cffi:foreign-slot-value
                        (cffi:foreign-slot-value event 'isys:epoll-event 'isys:data)
                        'isys:epoll-data 'isys:fd)
        if (= event-fd pipe-fd)
          do (add-to-wait pipe-fd)
        else
          do (dispatch-epoll-event event-fd event))
  (iomux::expire-pending-timers *timers* (isys:get-monotonic-time)))

(defun start-handler-thread (server fd-hash timers epoll-fd pipe-fd)
  (let ((*server* server)
        (*fd-hash* fd-hash)
        (*timers* timers)
        (*epoll-fd* epoll-fd)
        (*buffer* (make-array (slot-value server 'default-buffer-size) :element-type '(unsigned-byte 8))))
    (cffi:with-foreign-object (events 'isys:epoll-event iomux::+epoll-max-events+)
      (isys:bzero events (* iomux::+epoll-max-events+ isys:size-of-epoll-event))
      (loop for ready-fds = (handler-case (isys:epoll-wait *epoll-fd* events iomux::+epoll-max-events+
                                                           500)
                              (isys:eintr ()
                                (warn "epoll-wait EINTR")
                                0))
            do (handle-events events ready-fds pipe-fd)))))

(defun start-accept-thread (server)
  (let ((*server* server)
        (*fd-hash* (make-hash-table))
        (*timers* (iomux::make-priority-queue :key #'iomux::%timer-expire-time))
        (*epoll-fd* (isys:epoll-create 1)))
    (multiple-value-bind (pipe-read pipe-write) (sb-posix:pipe)
      (epoll-ctl pipe-read *epoll-fd* isys:epoll-ctl-add isys:epollin)
      (sb-thread:make-thread #'start-handler-thread
                             :name (format nil "handler of ~a"
                                           (sb-thread:thread-name sb-thread:*current-thread*))
                             :arguments (list *server* *fd-hash* *timers* *epoll-fd* pipe-read))
      (cffi:with-foreign-object (buf :int)
        (loop with listen-fd = (slot-value server 'listen-fd)
              for client-fd = (iolib.sockets::with-sockaddr-storage-and-socklen (ss size)
                                (iolib.sockets::%accept listen-fd ss size))
              do (setf (isys:fd-nonblock client-fd) t)
                 (setf (cffi:mem-aref buf :int 0) client-fd)
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


(defmethod normalize-env (server env buffer position fd)
  (with-slots (document-root) server
    (setf (gethash "DOCUMENT_ROOT" env) document-root)
    (let* ((request-uri (gethash "REQUEST_URI" env))
           (? (position #\? request-uri :start 1)))
      (setf (gethash "PATH_INFO" env) (subseq request-uri 0 ?))
      (setf (gethash "QUERY_STRING" env) (if ? (subseq request-uri (1+ ?)) "")))
    (setf (gethash :fd env) fd)
    #+nil
    (iterate (((k v) (scan-hash env)))
      (format t "~a: ~a~%" k v))
    env))

(defmethod parse-request (server fd buffer buffer-size env)
  (prog ()
     (remhash :remain-request-buffer env)
   :start
     (multiple-value-bind (ok position) (start-parse-request buffer 0 buffer-size env)
       (if ok
           (progn
             (normalize-env server env buffer position fd)
             (set-keep-alive-timer fd env)
             (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))
           (let ((remain-size (- buffer-size position)))
             (cond ((zerop position)
                    ;; buffer full
                    (let* ((new-buffer-size (* 2 (length buffer)))
                           (new-buffer (make-array new-buffer-size :element-type '(unsigned-byte 8))))
                      (iterate ((i (scan-range :below buffer-size)))
                        (setf (aref new-buffer i) (aref buffer i)))
                      (setf *buffer* new-buffer))
                    (let ((read-size (receive fd *buffer* remain-size (length *buffer*))))
                      (unless read-size
                        (setf (gethash :remain-request-buffer env) (subseq *buffer* 0 remain-size))
                        (return))
                      (setf buffer *buffer*)
                      (setf buffer-size (+ read-size remain-size))
                      (go :start)))
                   (t
                    ;; TODO この場合も上と同じように *buffer* を大きくする。
                    (memmove buffer position buffer-size)
                    (let ((read-size (receive fd buffer remain-size (length buffer))))
                      (unless read-size
                        (setf (gethash :remain-request-buffer env) (subseq buffer 0 remain-size))
                        (return))
                      (setf buffer-size (+ read-size remain-size))
                      (go :start)))))))))


(defmethod receive-request (server fd)
  (let* ((env (gethash fd *fd-hash*))
         (start (let* ((buf (gethash :remain-request-buffer env #()))
                       (len (length buf)))
                  (iterate ((i (scan-range :length len)))
                    (setf (aref *buffer* i) (aref buf i)))
                  len))
         (read-size (receive fd *buffer* start (length *buffer*))))
    (if (plusp read-size)
        (parse-request server fd *buffer* (+ start read-size) env)
        (close-connection server fd))))

(defvar *env*)

(defun keep-alive-p (env)
  ;; TODO Connection ヘッダ
  (and
   (equal (gethash "SERVER_PROTOCOL" env) "HTTP/1.1")
   (plusp (slot-value *server* 'keep-alive-timeout))))

(defmethod send-response (server fd)
  (with-slots (document-root handler) server
    (let ((*env* (gethash fd *fd-hash*)))
      (handle-request handler server fd *env*)
      (if (keep-alive-p *env*)
          (progn
            (remhash :parse-function *env*)
            (remhash :remain-request-buffer *env*)
            (iterate (((k v) (scan-hash *env*)))
              (declare (ignore v))
              (when (stringp k)
                (remhash k *env*)))
            (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollin isys:epollhup epollet))
          (close-connection server fd)))))

(defun status-message (status)
  (case status
    (200 "OK")
    (404 "Not Found")
    (t "que")))

(defmethod write-response ((string string))
  (let ((fd (gethash :fd *env*)))
    (cffi:with-foreign-string ((s length) string :encoding :utf-8 :null-terminated-p nil)
      (isys:write fd s length))))

(defmethod write-response (vector)
  (let ((fd (gethash :fd *env*)))
    (cffi-sys:with-pointer-to-vector-data (pointer vector)
      (isys:write fd pointer (length vector)))))

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
         (st (isys:fstat file-fd))
         (last-modified (rfc-2822-posix (isys:stat-mtime st)))
         (content-length (isys:stat-size st)))
    (with-cork (fd)
      (cffi:with-foreign-string ((s length)
                                 (format nil "HTTP/1.1 200 OK~a~
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
                                 :null-terminated-p nil)
        (isys:write fd s length))
      (%sendfile fd file-fd (cffi-sys:null-pointer) content-length))
    (isys:close file-fd)
    t))

(defun %send-response (status headers body)
  (let ((fd (gethash :fd *env*)))
    (aif (cdr (assoc "X-Sendfile" headers :test #'equal))
         (unless (sendfile fd it)
           (error "X-Sendfile not found"))
         (with-cork (fd)
           (write-response (format nil "HTTP/1.1 ~d ~a~a" status (status-message status) +crlf+))
           (write-response (collect-append
                            'string
                            (multiple-value-bind (k v) (scan-alist headers #'equal)
                              (format nil "~a: ~a~a" k v +crlf+))))
           (write-response +crlf+)
           (collect-ignore (write-response (scan body)))
           t))))


(defmethod close-connection (server fd)
  (let ((env (gethash fd *fd-hash*)))
    (awhen (and env (gethash :keep-alive-timer env))
      (iomux::unschedule-timer *timers* it)))
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

(defmethod handle-request or ((handler 404-handler) server fd env)
  (let ((body (string-to-octets (concatenate 'string "<html>
<head><title>not found</title></head>
<body>Not Found. " (h (gethash "PATH_INFO" env)) "</body></html>"))))
    (%send-response 404 `(("Content-type" . "text/html")
                          ("Date" . ,(rfc-2822-now))
                          ("Content-Length" . ,(length body)))
                    (list body))))


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
                                    (choose
                                     (multiple-value-bind (k v) (scan-hash env)
                                       (when (stringp k)
                                         (format nil "~a=~a" k v)))))))))
        (write-response (format nil "HTTP/1.1 200 OK~a" +crlf+))
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
