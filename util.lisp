(in-package :info.read-eval-print.httpd)

(deftype octet ()
  '(unsigned-byte 8))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet))

(defun octet-vector (&rest octets)
  (make-array (length octets) :element-type 'octet :initial-contents octets))

(alexandria:define-constant +crlf+ (format nil "~c~c" #\cr #\lf) :test #'equalp)

(defun string-to-octets (string &key (external-format :utf-8)
                                  (start 0) end null-terminate)
  (sb-ext:string-to-octets string :external-format external-format
                                  :start start
                                  :end end
                                  :null-terminate null-terminate))

(defun octets-to-string (vector &key (external-format :utf-8) (start 0) end)
  (sb-ext:octets-to-string (coerce vector '(vector (unsigned-byte 8) *))
                           :external-format external-format :start start :end end))

(defun memmove (buffer start end)
  (cffi-sys:with-pointer-to-vector-data (pointer buffer)
    (iolib.syscalls:memmove pointer
                            (cffi-sys:inc-pointer pointer start)
                            (- end start))))

(defun send (socket-fd buffer start end)
  (let ((length (- end start)))
    (cffi-sys:with-pointer-to-vector-data (pointer buffer)
      (iolib.sockets::%sendto socket-fd
                              (cffi-sys:inc-pointer pointer start)
                              length
                              0
                              (cffi-sys:null-pointer)
                              0))))

(defun receive (socket-fd buffer start end)
  (let ((length (- end start)))
    (cffi-sys:with-pointer-to-vector-data (pointer buffer)
      (handler-case (iolib.sockets::%recvfrom socket-fd
                                              (cffi:inc-pointer pointer start)
                                              length
                                              0
                                              (cffi:null-pointer)
                                              (cffi:null-pointer))
        (iolib.syscalls:ewouldblock ()
          nil)))))

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

(defparameter *mime-types*
  (alexandria:alist-hash-table
   `(("html" . "text/html")
     ("htm" . "text/html")
     ("txt" . "text/plain")
     ("png" . "image/png")
     ("gif" . "image/gif")
     ("jpeg" . "image/jpeg")
     ("jpg" . "image/jpeg")
     ("pdf" . "application/pdf")
     ("ps" . "application/postscript")
     ("zip" . "application/zip")
     ("css" . "text/css")
     ("js" . "application/x-javascript"))
   :test #'equalp))

(defun mime-type (type)
  (gethash type *mime-types* "application/octet-stream"))

(defun path-mime-type (path)
  (aif (position #\. path :from-end t)
       (mime-type (subseq path (1+ it)))
       "application/octet-stream"))


(alexandria:define-constant +week-names+ #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  :test #'equalp)

(alexandria:define-constant +month-names+ #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                            "Aug" "Sep" "Oct" "Nov" "Dec")
  :test #'equalp)

(defmacro %rfc-2822 (time)
  `(multiple-value-bind (second minute hour date month year day daylight-p zone) ,time
     (declare (ignore daylight-p))
     (format nil "~a, ~02,'0d ~a ~04,'0d ~02,'0d:~02,'0d:~02,'0d ~a~02,'0d00"
             (aref +week-names+ day)
             date
             (aref +month-names+ (1- month))
             year
             hour
             minute
             second
             (if (minusp zone) "+" "-")
             (abs zone))))

(declaim (inline rfc-2822-now))
(defun rfc-2822-now ()
  "Sun, 19 Aug 2012 13:14:43 +0000"
  (%rfc-2822 (get-decoded-time)))

(declaim (inline rfc-2822))
(defun rfc-2822 (universal-time)
  "Sun, 19 Aug 2012 13:14:43 +0000"
  (%rfc-2822 (decode-universal-time universal-time)))

(defconstant +posix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(declaim (inline rfc-2822-posix))
(defun rfc-2822-posix (posix-time)
  (%rfc-2822 (decode-universal-time (+ posix-time +posix-epoch+))))

(defvar %%%%lock (sb-thread:make-mutex))

(defmacro with-global-lock (&body body)
  `(sb-thread:with-recursive-lock (%%%%lock)
     ,@body))

(defstruct queue
  head
  tail)

(defun enqueue (queue x)
  (let ((cons (cons x nil)))
    (if (null (queue-tail queue))
        (setf (queue-head queue) cons)
        (setf (cdr (queue-tail queue)) cons))
    (setf (queue-tail queue) cons)))

(defun dequeue (queue)
  (prog1 (car (queue-head queue))
    (if (eq (queue-head queue) (queue-tail queue))
        (setf (queue-head queue) nil
              (queue-tail queue) nil)
        (setf (queue-head queue) (cdr (queue-head queue))))))

(defun queue-empty-p (queue)
  (queue-head queue))
