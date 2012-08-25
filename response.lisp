(in-package :info.read-eval-print.httpd)

#|
accept
read request
app
  response stream
    epoll_wait IN
  app queue
  thread pool
|#

(defgeneric start-response (stream))

(defgeneric %write-to-fd (stream))

(defclass response-stream (trivial-gray-streams:trivial-gray-stream-mixin
                           trivial-gray-streams:fundamental-binary-output-stream
                           trivial-gray-streams:fundamental-character-output-stream)
  ((fd :initarg :fd :accessor fd-of)
   (external-format :initarg :external-format :initform :utf-8
                    :accessor external-format-of)
   (response-status :initarg :response-status :initform 200
                    :accessor response-status-of)
   (response-headers :initarg :respone-headens
                     :initform (list (cons "Date" (rfc-2822-now))
                                     (cons "Content-Type" "text/html; charset=UTF-8")
                                     (cons "Transfer-Encoding" "chunked"))
                     :accessor response-headers-of)
   (started :initform nil :accessor started-p)))

(defmethod trivial-gray-streams:stream-force-output ((stream response-stream))
  (%write-last-chunk (fd-of stream)))

(defmethod trivial-gray-streams:stream-write-sequence :before ((stream response-stream)
                                                               sequence start end &key)
  (unless (started-p stream)
    (start-response stream)))

(declaim (inline %write-chunk))
(defun %write-chunk (fd buffer size)
  (unless (zerop size)
    (%format-to-fd fd :latin-1 "~X~a" size +crlf+)
    (isys:write fd buffer size)
    (%format-to-fd fd :latin-1 +crlf+)))

(declaim (inline %write-last-chunk))
(defun %write-last-chunk (fd)
  (%format-to-fd fd :latin-1 "0~a~a" +crlf+ +crlf+)
  (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 0))

(declaim (inline write-chunk))
(defun write-chunk (stream buffer size)
  (unless (started-p stream)
    (start-response stream))
  (%write-chunk (fd-of stream) buffer size))

(defmethod trivial-gray-streams:stream-write-sequence ((stream response-stream)
                                                       (string string) start end &key)
  (cffi:with-foreign-string ((s length) string
                             :encoding (external-format-of stream)
                             :null-terminated-p nil
                             :start start :end end)
    (write-chunk stream s length)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream response-stream)
                                                       sequence start end &key)
  (cffi-sys:with-pointer-to-vector-data (ptr sequence)
    (cffi-sys:inc-pointer ptr start)
    (write-chunk stream ptr (- end start))))

(defmethod trivial-gray-streams:stream-write-string :before ((stream response-stream) string
                                                             &optional start end)
  (declare (ignore start end))
  (unless (started-p stream)
    (start-response stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream response-stream) (string string)
                                                     &optional (start 0) end)
  (cffi:with-foreign-string ((s length) string
                             :encoding (external-format-of stream)
                             :null-terminated-p nil
                             :start start :end (or end (length string)))
    (write-chunk stream s length)))

(defmethod trivial-gray-streams:stream-write-char :before ((stream response-stream) character)
  (unless (started-p stream)
    (start-response stream)))

(defmethod trivial-gray-streams:stream-write-char ((stream response-stream) character)
  (trivial-gray-streams:stream-write-sequence stream (make-string 1 :initial-element character)
                                              0 1))


(defmethod response-message-of ((stream response-stream))
  (case (response-status-of stream)
    (200 "OK")
    (404 "Not Found")
    (t "que")))

(defmethod start-response ((stream response-stream))
  (let ((fd (fd-of stream))
        (external-format (external-format-of stream)))
    (iolib.sockets::set-socket-option-int fd iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 1)
    (%format-to-fd fd
                   external-format
                   "HTTP/1.1 ~d ~a~a"
                   (response-status-of stream)
                   (response-message-of stream)
                   +crlf+)
    (iterate (((k v) (scan-alist (response-headers-of stream))))
      (%format-to-fd fd external-format
                     "~a: ~a~a" k v +crlf+))
    (%format-to-fd fd external-format +crlf+)
    (setf (started-p stream) t)))


(defun %format-to-fd (fd exteral-format format &rest args)
  (cffi:with-foreign-string ((s length) (apply #'format nil format args)
                             :encoding exteral-format
                             :null-terminated-p nil)
    (isys:write fd s length)))
