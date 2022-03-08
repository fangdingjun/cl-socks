(in-package :socks)

(defun connect (host port dst-host dst-port)
  (let* ((sock (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
         (s1 (usocket:socket-stream sock)))

    (write-bytes-n s1 '(#x5 #x0))
    (if (/= (second (read-bytes-n s1 2)) 0)
        (progn
         (usocket:socket-close sock)
         (return-from connect nil)))
    (write-bytes-n s1 '(#x5 #x1 #x0))
    (write-host-port s1 dst-host dst-port)
    (if (/= (second (read-bytes-n s1 10)) 0)
        (progn
         (usocket:socket-close socks)
         (return-from connect nil)))
    sock))

(defun write-host-port (out host port)
  (cond
   ((usocket::ip-address-string-p host)
    (write-byte #x1 out) ; ipv4
    (write-uint32 out (usocket:host-byte-order host))
    (write-uint16 out port))
   (t
    (write-byte #x3 out) ;domain
    (let ((s (string-to-octets host)))
      (write-byte (length s) out)
      (write-bytes-n out s))
    (write-uint16 out port))))

(defun string-to-octets (s)
  (let ((tt (if (vectorp s) 'vector nil)))
    (map tt #'char-code s)))
