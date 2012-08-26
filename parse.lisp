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
             (values t (+ start 4) #'parse-request-uri))
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
  (declare (ignore request))
  (cond ((= start end)
         (values nil start #'parse-header-end))
        ((/= #x0a (aref buffer start))
         (error 'invalid-request))
        (t
         (values t (1+ start) nil))))

