(in-package :info.read-eval-print.httpd)

(defun start-parse-request (buffer start end request)
  (with-slots (parse-function) request
   (prog ((fun (or parse-function #'parse-method)))
    :restart
      (multiple-value-bind (ok position next) (funcall fun buffer start end request)
        (if ok
            (if next
                (setf fun next start position)
                (return (values t position nil)))
            (progn
              (setf parse-function next)
              (return (values nil position)))))
      (go :restart))))

(define-condition invalid-request () ())

(defmacro request-method-matche-p (method buffer start)
  `(and ,@(loop for i below (length method)
                collect `(= ,(char-code (aref method i)) (aref ,buffer (+ ,i ,start))))
        (= #x20 (aref ,buffer (+ ,(length method) ,start)))))

(defun parse-method (buffer start end request)
  (if (< (- end start) #.(length "GET / HTTP/1.1"))
      (values nil start #'parse-method)
      (cond ((request-method-matche-p "GET" buffer start)
             (setf (env request :method) :get)
             (values t (+ start 4) #'parse-request-uri))
            ((request-method-matche-p "POST" buffer start)
             (setf (env request :method) :post)
             (values t (+ start 5) #'parse-request-uri))
            (t (error 'invalid-request)))))

(defun parse-request-uri (buffer start end request)
  (aif (position #x20 buffer :start start :end end)
       (progn
         (setf (env request :request-uri) (octets-to-string buffer :start start :end it))
         (values t (1+ it) #'parse-protocol))
       (values nil start #'parse-request-uri)))

(defun request-class-from-protocol (protocol)
  (cond ((equal protocol "HTTP/1.1")
         'http-1.1-request)
        ((equal protocol "HTTP/1.0")
         'http-1.0-request)
        ((equal protocol "HTTP/0.9")
         'http-0.9-request)
        (t (error 'invalid-request))))

(defun parse-protocol (buffer start end request)
  (aif (position #x0d buffer :start start :end end)
       (let ((protocol (octets-to-string buffer :start start :end it)))
         (setf (env request :server-protocol) protocol)
         (change-class request (request-class-from-protocol protocol))
         (values t (1+ it) #'parse-header))
       (values nil start #'parse-protocol)))

(defun store-header (buffer start end request)
  (let ((colon (position #.(char-code #\:) buffer :start start :end end)))
    (unless colon
      (error 'invalid-request))
    (let* ((name (octets-to-string buffer :start start :end colon))
           (value (octets-to-string buffer :start (1+ colon) :end end))
           (key (intern (concatenate 'string "HTTP-" (substitute #\- #\_ (string-upcase name)))
                        :keyword)))
      (setf (env request key) (string-left-trim #(#\space) value)))))

(defun parse-header (buffer start end request)
  (prog ()
   :start
     (when (= start end)
       (return (values nil start #'parse-header)))
     (when (/= #x0a (aref buffer start))
       (error 'invalid-request))
     (aif (position #x0d buffer :start (incf start) :end end)
          (if (= it start)
              (return (values t (1+ start) #'parse-header-end))
              (progn
                (store-header buffer start it request)
                (setf start (1+ it))
                (go :start)))
          (return (values nil (1- start) #'parse-header)))))

(defun parse-header-end (buffer start end request)
  (cond ((= start end)
         (values nil start #'parse-header-end))
        ((/= #x0a (aref buffer start))
         (error 'invalid-request))
        ((not (eq (env request :method) :post))
         (values t (1+ start) nil))
        (t
         (incf start)
         (if (string= (env request :http-transfer-encoding) "chunked")
             (parse-chunked-post-data buffer start end request)
             (pares-length-post-data buffer start end request)))))

(defun parse-chunked-post-data (buffer start end request)
  (with-slots (post-data) request
    (setf post-data (make-array 4096 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer t))
    (parse-chunked-post-data-length buffer start end request)))

(defun parse-chunked-post-data-length (buffer start end request)
  (aif (search #(#x0d #x0a) buffer :start2 start :end2 end)
       (progn
         (let ((len (parse-integer (octets-to-string buffer :start start :end it)
                                   :radix 16)))
           (if (zerop len)
               (parse-chunked-post-data-end buffer (+ it 2) end request)
               (funcall (make-parse-chunked-post-data-data len)
                        buffer (+ it 2) end request))))
       (values nil start #'parse-chunked-post-data-length)))

(defun make-parse-chunked-post-data-data (len)
  (lambda (buffer start end request)
    (loop with post-data = (slot-value request 'post-data)
          do (cond ((= start end)
                    (return (values nil start (make-parse-chunked-post-data-data len))))
                   ((zerop len)
                    (return (parse-chunked-post-data-data-end buffer start end request)))
                   (t
                    (vector-push-extend (aref buffer start) post-data)
                    (incf start)
                    (decf len))))))

(defun parse-chunked-post-data-data-end (buffer start end request)
  (if (> (+ 2 start) end)
      (values nil start #'parse-chunked-post-data-data-end)
      (if (and (= #x0d (aref buffer start))
               (= #x0a (aref buffer (1+ start))))
          (parse-chunked-post-data-length buffer (+ 2 start) end request)
          (error 'invalid-request))))

(defun parse-chunked-post-data-end (buffer start end request)
  (declare (ignore request))
  (aif (search #(#x0d #x0a) buffer :start2 start :end2 end)
       (values t (+ it 2) nil)
       (values nil start #'parse-chunked-post-data-end)))

(defun pares-length-post-data (buffer start end request)
  (with-slots (post-data) request
    (let ((length (parse-integer (env request :http-content-length))))
      (setf post-data (make-array length :element-type '(unsigned-byte 8)))
      (funcall (make-parse-length-post-data-data 0 length)
               buffer start end request))))

(defun make-parse-length-post-data-data (start1 end1)
  (lambda (buffer start2 end2 request)
    (let ((post-data (slot-value request 'post-data)))
      (replace post-data buffer
               :start1 start1
               :end1 end1
               :start2 start2
               :end2 end2)
      (if (<= (- end1 start1) (- end2 start2))
          (values t end2 nil)
          (values nil end2 (make-parse-length-post-data-data (+ start1 (- end2 start2)) end1))))))

(defun make-parse-post-data (read-length content-length)
  (lambda (buffer start end request)
    (with-slots (post-data) request
      (replace post-data buffer
               :start1 start
               :end1 end
               :start2 read-length
               :end2 content-length)
      (if (<= (- content-length read-length) (- end start))
          (values t end nil)
          (values nil end (make-parse-post-data (+ read-length (- end start))
                                                content-length))))))


