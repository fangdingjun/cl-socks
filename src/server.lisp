(in-package :socks)

(defun read-uint16 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in nil 0))
    (setf (ldb (byte 8 0) u2) (read-byte in nil 0))
    u2))

(defun write-uint16 (out num)
  (write-byte (ldb (byte 8 8) num) out)
  (write-byte (ldb (byte 8 0) num) out)
  (force-output out))

(defun read-uint32 (in)
  (let ((u32 0))
    (setf (ldb (byte 8 24) u32) (read-byte in nil 0))
    (setf (ldb (byte 8 16) u32) (read-byte in nil 0))
    (setf (ldb (byte 8 8) u32) (read-byte in nil 0))
    (setf (ldb (byte 8 0) u32) (read-byte in nil 0))
    u32))

(defun write-uint32 (out num)
  (write-byte (ldb (byte 8 24) num) out)
  (write-byte (ldb (byte 8 16) num) out)
  (write-byte (ldb (byte 8 8) num) out)
  (write-byte (ldb (byte 8 0) num) out)
  (force-output out))

(defun to-ip (u32)
  (format nil "~d.~d.~d.~d"
          (ldb (byte 8 24) u32)
          (ldb (byte 8 16) u32)
          (ldb (byte 8 8) u32)
          (ldb (byte 8 0) u32)))

(defun read-version (in)
  (read-byte in nil 0))

(defun read-auth (in)
  (let ((n (read-byte in nil 0)))
    (read-bytes-n in n)))

(defun _forward (in out)
  (loop for b = (read-byte in nil nil)
        while b
        do (write-byte b out)
           (force-output out)))

(defun forward (out in)
  (bt:make-thread (lambda ()
                    (_forward out in)))
  (_forward in out))

(defun read-bytes-n (in n)
  (if (= n 0) (return-from read-bytes-n nil))
  (let ((ch (read-byte in nil nil)))
    (if (null ch) (return-from read-bytes-n nil))
    (cons ch (read-bytes-n in (- n 1)))))

(defun write-bytes-n (out data)
  (if (null data)
      (force-output out)
      (progn
       (write-byte (car data) out)
       (write-bytes-n out (cdr data)))))

;(eval-when (:compile-toplevel)
(defun handshake (in out)
  (let ((ver (read-version in)))
    (cond ((eq ver 5)
           (read-auth in)
           (write-bytes-n out '(#x5 #x0))
           (handle-command in out))
          (t (format *standard-output* "unsupported version ~a~%" ver)))))

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

    (format *standard-output* "destination ~a~% port ~a~%" host port)

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

(defun create-connection (host port)
  (usocket:socket-connect host port :element-type '(unsigned-byte 8) :protocol :stream :nodelay t))

(defun listen-for-connection (host port)
  (let ((sock (usocket:socket-listen host port :reuse-address t :element-type '(unsigned-byte 8))))
    (bt:make-thread (lambda ()
                      (accept-connection sock)
                      (format *standard-output* "accept thread ended~%")))
    sock))

(defun _handle-connection (sock)
  (unwind-protect
   (let ((sock1 (usocket:socket-stream sock)))
     (handshake sock1 sock1))
   (progn
    (format *standard-output* "close in socket ~%")
    (usocket:socket-close sock))))

(defun handle-connection (sock)
  (bt:make-thread (lambda ()
                    (_handle-connection sock))))

(defun accept-connection (sock)
  (loop for sock1 = (usocket:socket-accept sock :element-type '(unsigned-byte 8))
        while sock1
        do (handle-connection sock1)));)   ; end eval-when
