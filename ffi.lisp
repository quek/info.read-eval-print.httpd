(in-package :info.read-eval-print.httpd)

(isys:defsyscall (%sendfile "sendfile") :int
  (out-fd :int)
  (in-fd :int)
  (offset (:pointer isys::off-t))
  (count isys::size-t))

(isys:defsyscall (%socketpair "socketpair") :int
  (domain :int)
  (type :int)
  (protocol :int)
  (sv (:pointer :int)))

(defun socketpair ()
  (cffi:with-foreign-object (sv :int 2)
    (%socketpair iolib.sockets::af-local iolib.sockets::sock-dgram 0 sv)
    (values (cffi:mem-aref sv :int)
            (cffi:mem-aref sv :int 1))))