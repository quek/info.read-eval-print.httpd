(in-package :info.read-eval-print.httpd)

(defvar *thread-local*)

(defstruct thread-local
  (server)
  (fd-hash (make-hash-table))
  (timers (iomux::make-priority-queue :key #'iomux::%timer-expire-time))
  (epoll-fd (isys:epoll-create 1))
  (buffer (make-octet-vector 4096))
  (dispatch-in-table (make-hash-table))
  (dispatch-out-table (make-hash-table))
  (dispatch-hup-table (make-hash-table))
  (pipe-write-fd))
