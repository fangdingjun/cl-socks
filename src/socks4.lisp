(in-package :socks)

(defun handle-socks4 (conn)
  (let ((cmd (read-byte conn nil 0)))
    (cond ((eq cmd #x1)
           (handle-socks4-connect conn))
          (t (return-from handle-socks4 nil)))))

(defun read-null-string (in)
  (loop for ch = (read-byte in nil 0)
        while (/= ch 0)
        collect ch))

(defun handle-socks4-connect (conn)
  (let ((port (read-uint16 conn))
        (host (read-uint32 conn)))

    ; user
    (read-null-string conn)

    ; 4a,  ip 0.0.0.x  read hostname
    (if (<= host 256)
        (setf host (flexi-streams:octets-to-string (read-null-string conn))))

    (format *standard-output* "socks4a destination ~a port ~a~%" host port)

    (write-bytes-n conn '(#x00 #x5a #x00 #x00 #x00 #x00 #x00 #x00))

    (let ((out1 (create-connection host port)))
      (unwind-protect
       (forward conn (usocket:socket-stream out1))
       (progn
        (format *standard-output* "close out socket~%")
        (usocket:socket-close out1))))))
