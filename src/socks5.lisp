(in-package :socks)

(defun handle-connect-command (in out)
  (read-byte in nil nil)
  (let ((addr-type (read-byte in nil 0))
        (host 0)
        (port 0))
    (cond ((eq addr-type 1) ;ipv4
           (setf host (read-uint32 in))
           (setf port (read-uint16 in))
           (format *standard-output* "ipv4 ~a~%" (to-ip host)))
          ((eq addr-type 3) ; domain
           (format *standard-output* "domain~%")
           (let ((n (read-byte in nil 0)))
             (setf host (flexi-streams:octets-to-string (read-bytes-n in n)))
             (setf port (read-uint16 in))))
          ((eq addr-type 4) ; ipv6
           (format *standard-output* "ipv6~%")
           (setf host (read-bytes-n in 16))
           (setf port (read-uint16 in)))
          (t
           (format *standard-output* "unsupported addr type ~a~%" addr-type)
           (return-from handle-connect-command nil)))

    (format *standard-output* "destination ~a port ~a~%" host port)

    (write-bytes-n out '(#x5 #x0 #x0 #x1 #x0 #x0 #x0 #x0 #x0 #x0))
    (let ((out1 (create-connection host port)))
      (unwind-protect
       (forward in (usocket:socket-stream out1))
       (progn
        (format *standard-output* "close out socket~%")
        (usocket:socket-close out1))))))

(defun handle-command (in out)
  (read-version in)
  (let ((cmd (read-byte in nil 0)))
    (cond ((eq cmd 1) (handle-connect-command in out))
          (t (format *standard-output* "unsupported command ~a~%" cmd)))))