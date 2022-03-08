(defpackage :test/all
  (:use :cl :socks :fiveam)
  (:export :test-suite))

(in-package :test/all)

(def-suite all-tests :description "test")

(in-suite all-tests)

(test test-socks
  (multiple-value-bind (t1 srv)
      (socketserver:create-server "0.0.0.0" 3458 #'socketserver:handler)
    (multiple-value-bind (t2 sock)
        (socks:listen-for-connection "0.0.0.0" 3459)
      (let ((s)
            (s1))
        (setf s (socks:connect "127.0.0.1"
                               (usocket:get-local-port sock)
                               "debian"
                               (usocket:get-local-port srv)))

        (setf s1 (usocket:socket-stream s))

        (format t "socks port ~a~%" (usocket:get-local-port sock))
        (format t "server port ~a~%" (usocket:get-local-port srv))

        (unwind-protect
         (progn
          (socks:write-bytes-n s1 '(#x1 #x2 #x3 #x21 #x48 #x90))
          (is (= (nth 5 (socks:read-bytes-n s1 6)) #x90)))

         (progn
          (usocket:socket-close srv)
          (usocket:socket-close s)
          (usocket:socket-close sock)
          (bt:destroy-thread t1)
          (bt:destroy-thread t2)
          (format t "close all sockets done~%")))))))

(defun test-suite ()
  (run! 'all-tests)
  (format t "test finished"))
