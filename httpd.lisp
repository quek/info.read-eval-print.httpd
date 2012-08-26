(in-package :info.read-eval-print.httpd)

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

(defgeneric handle-request (handler server fd request)
  (:method-combination or))

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
   (keep-alive-timeout :initarg :keep-alive-timer :initform 10)
   (quit :initform nil :accessor quit-p)
   (accept-threads :initform nil)))

(defmethod (setf quit-p) :after (value (server server))
  (when value
    (print 'closing)
    (close (slot-value server 'listen-socket))
    (print 'closed)
    (dolist (thread (slot-value server 'accept-threads))
      (print thread)
      (sb-thread:terminate-thread thread))))

(defvar *server*)
(defvar *fd-hash*)
(defvar *timers*)
(defvar *epoll-fd*)
(defvar *buffer*)
(defvar *accept-thread-fd*)
(defvar *request*)

(defun set-keep-alive-timer (fd request)
  (with-slots (keep-alive-timer) request
    (when (and (keep-alive-p request) (not keep-alive-timer))
      (let ((timer (iomux::make-timer
                    (lambda ()
                      (print (list fd "close keep alive connection!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
                      (close-connection *server* fd))
                    (slot-value *server* 'keep-alive-timeout)
                    :one-shot t)))
        (setf keep-alive-timer timer)
        (iomux::schedule-timer *timers* timer)))))

(defun add-to-epoll-wait (pipe-fd)
  (cffi:with-foreign-object (buf :int) (sb-posix:read pipe-fd buf #.(cffi:foreign-type-size :int))
    (let ((fd (cffi:mem-aref buf :int 0)))
      (sif (gethash fd *fd-hash*)
           (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollin isys:epollhup epollet)
           (progn
             (setf it (make-instance 'request :fd fd))
             (epoll-ctl fd *epoll-fd* isys:epoll-ctl-add isys:epollin isys:epollhup epollet))))))

(defun dispatch-epoll-event (fd event)
  (let ((event-mask (cffi:foreign-slot-value event 'isys:epoll-event 'isys:events)))
    (cond ((logtest event-mask isys:epollin)
           (receive-request *server* fd))
          ((logtest event-mask isys:epollout)
           (send-response *server* fd))
          ((logtest event-mask isys:epollhup)
           (close-connection *server* fd)))))

;; (declaim (inline handle-events))
(defun handle-events (events ready-fds pipe-read-fd)
  (loop for i below ready-fds
        for event = (cffi:mem-aref events 'isys:epoll-event i)
        for event-fd = (cffi:foreign-slot-value
                        (cffi:foreign-slot-value event 'isys:epoll-event 'isys:data)
                        'isys:epoll-data 'isys:fd)
        if (= event-fd pipe-read-fd)
          do (add-to-epoll-wait pipe-read-fd)
        else
          do (dispatch-epoll-event event-fd event))
  (iomux::expire-pending-timers *timers* (isys:get-monotonic-time)))

(defun start-handler-thread (server fd-hash timers epoll-fd pipe-read-fd pipe-write-fd)
  (let ((*server* server)
        (*fd-hash* fd-hash)
        (*timers* timers)
        (*epoll-fd* epoll-fd)
        (*buffer* (make-array (slot-value server 'default-buffer-size) :element-type '(unsigned-byte 8)))
        (*accept-thread-fd* pipe-write-fd))
    (cffi:with-foreign-object (events 'isys:epoll-event iomux::+epoll-max-events+)
      (isys:bzero events (* iomux::+epoll-max-events+ isys:size-of-epoll-event))
      (loop for ready-fds = (handler-case (isys:epoll-wait *epoll-fd* events iomux::+epoll-max-events+
                                                           500)
                              (isys:eintr ()
                                (warn "epoll-wait EINTR")
                                0))
            until (quit-p server)
            do (handle-events events ready-fds pipe-read-fd)))))

(defun start-accept-thread (server)
  (let ((*server* server)
        (*fd-hash* (make-hash-table))
        (*timers* (iomux::make-priority-queue :key #'iomux::%timer-expire-time))
        (*epoll-fd* (isys:epoll-create 1)))
    (multiple-value-bind (pipe-read-fd pipe-write-fd) (sb-posix:pipe)
      (epoll-ctl pipe-read-fd *epoll-fd* isys:epoll-ctl-add isys:epollin)
      (sb-thread:make-thread #'start-handler-thread
                             :name (format nil "handler of ~a"
                                           (sb-thread:thread-name sb-thread:*current-thread*))
                             :arguments (list *server* *fd-hash* *timers* *epoll-fd*
                                              pipe-read-fd pipe-write-fd))
      (cffi:with-foreign-object (buf :int)
        (loop with listen-fd = (slot-value server 'listen-fd)
              for client-fd = (iolib.sockets::with-sockaddr-storage-and-socklen (ss size)
                                (iolib.sockets::%accept listen-fd ss size))
              until (quit-p server)
              do (setf (isys:fd-nonblock client-fd) t)
                 (setf (cffi:mem-aref buf :int 0) client-fd)
                 (sb-posix:write pipe-write-fd buf #.(cffi:foreign-type-size :int)))))))

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
               (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollout isys:epollhup epollet))
             ;; TODO buffer の上限。
             (let* ((remain-size (- buffer-size position))
                    (new-buffer-size (* 2 (length buffer)))
                    (new-buffer (make-array new-buffer-size :element-type '(unsigned-byte 8))))
               (iterate ((si (scan-range :from position :below buffer-size))
                         (di (scan-range)))
                 (setf (aref new-buffer di) (aref buffer si)))
               (setf *buffer* new-buffer)
               (let ((read-size (receive fd *buffer* remain-size new-buffer-size)))
                 (unless read-size
                   (setf remain-request-buffer (subseq *buffer* 0 remain-size))
                   (return))
                 (setf buffer new-buffer)
                 (setf buffer-size (+ read-size remain-size))
                 (go :start))))))))


(defmethod receive-request (server fd)
  (let ((request (gethash fd *fd-hash*)))
    (with-slots (remain-request-buffer) request
      (let* ((start (let* ((buf remain-request-buffer)
                           (len (length buf)))
                      (iterate ((i (scan-range :length len)))
                        (setf (aref *buffer* i) (aref buf i)))
                      (setf remain-request-buffer nil)
                      len))
             (read-size (handler-case (receive fd *buffer* start (length *buffer*))
                          (iolib.syscalls:econnreset () -1))))
        (if (plusp read-size)
            (parse-request server fd *buffer* (+ start read-size) request)
            (close-connection server fd))))))

(defun reset-client-fd-for-keep-alive (fd)
  (reset-request *request*)
  (epoll-ctl fd *epoll-fd* isys:epoll-ctl-mod isys:epollin isys:epollhup epollet))

(defmethod send-response (server fd)
  (with-slots (document-root handler) server
    (let ((*request* (gethash fd *fd-hash*)))
      (handle-request handler server fd *request*))))

(defun status-message (status)
  (case status
    (200 "OK")
    (404 "Not Found")
    (t "que")))

(defmethod write-response ((string string))
  (with-slots  (fd) *request*
    (cffi:with-foreign-string ((s length) string :encoding :utf-8 :null-terminated-p nil)
      (isys:write fd s length))))

(defmethod write-response (vector)
  (with-slots  (fd) *request*
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
  (with-slots (fd) *request*
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
  (let ((*request* (gethash fd *fd-hash*)))
    (awhen (and *request* (slot-value *request* 'keep-alive-timer))
      (iomux::unschedule-timer *timers* it)))
  (remhash fd *fd-hash*)
  (epoll-ctl fd *epoll-fd* isys:epoll-ctl-del)
  (isys:close fd))

(defclass after-handle-request-mixin ()
  ())

(defmethod handle-request :around ((handler after-handle-request-mixin) server fd request)
  (call-next-method)
  (if (keep-alive-p request)
      (reset-client-fd-for-keep-alive fd)
      (close-connection server fd)))

(defclass sendfile-handler (after-handle-request-mixin)
  ())

(defmethod handle-request or ((handler sendfile-handler) server fd request)
  (let* ((path (concatenate 'string
                            (env request :document-root)
                            (env request :path-info))))
    (when (and (fad:file-exists-p path)
               (not (fad:directory-exists-p path)))
      (sendfile fd path))))

(defclass 404-handler (after-handle-request-mixin)
  ())

(defmethod handle-request or ((handler 404-handler) server fd request)
  (let ((body (string-to-octets (concatenate 'string "<html>
<head><title>not found</title></head>
<body>Not Found. " (h (env request :path-info)) "</body></html>"))))
    (%send-response 404 `(("Content-type" . "text/html")
                          ("Date" . ,(rfc-2822-now))
                          ("Content-Length" . ,(length body)))
                    (list body))))


(defclass cgi-handler (after-handle-request-mixin)
  ())

(defmethod handle-request or ((handler cgi-handler) server fd request)
  (let ((path (concatenate 'string (env request :document-root)
                           (env request :path-info))))
    (when (alexandria:ends-with-subseq ".cgi" path :test #'equal)
      (let ((body (with-output-to-string (body)
                    (sb-ext:run-program
                     "/bin/sh" (list path)
                     :output body
                     :environment (collect 'bag
                                    (choose
                                     (multiple-value-bind (k v) (scan-hash (slot-value request 'env))
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
