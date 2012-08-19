(in-package :info.read-eval-print.httpd)

(defun string-to-octets (string &key (external-format :utf-8)
                                  (start 0) end null-terminate)
  (sb-ext:string-to-octets string :external-format external-format
                                  :start start
                                  :end end
                                  :null-terminate null-terminate))

(defun octets-to-string (vector &key (external-format :utf-8) (start 0) end)
  (sb-ext:octets-to-string vector :external-format external-format :start start :end end))

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
     ("zip" . "application/zip"))
   :test #'equalp))

(defun mime-type (type)
  (gethash type *mime-types* "application/octet-stream"))

(defun path-mime-type (path)
  (aif (position #\. path :from-end t)
       (mime-type (subseq path (1+ it)))
       "application/octet-stream"))