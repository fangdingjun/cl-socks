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
    (loop for i from 1 to n
          collect (read-byte in nil 0))))

(defun _forward (in out)
  (loop for b = (read-byte in nil nil)
        while b
        do (write-byte b out)
           (force-output out))
  (format *standard-output* "_forward ended~%"))

(defun forward (in out)
  (let ((thread (bt:make-thread (lambda ()
                                  (_forward out in)))))
    (unwind-protect
     (_forward in out)
     (if (bt:thread-alive-p thread)
         (bt:destroy-thread thread)))))

(defun read-bytes-n (in n)
  (loop for i from 1 to n
        collect (read-byte in nil 0)))

(defun write-bytes-n (out data)
  (let ((tt (if (vectorp data) 'vector nil)))
    (map tt #'(lambda (ch) (write-byte ch out)) data)
    (force-output out)))

;(eval-when (:compile-toplevel)
(defun handshake (in out)
  (let ((ver (read-version in)))
    (cond ((eq ver 5)
           (read-auth in)
           (write-bytes-n out '(#x5 #x0))
           (handle-command in out))
          ((eq ver 4)
           (handle-socks4 in))
          (t (format *standard-output* "unsupported version ~a~%" ver)))))

(defun create-connection (host port)
  (usocket:socket-connect host port :element-type '(unsigned-byte 8) :protocol :stream :nodelay t))

(defun listen-for-connection (host port)
  (let ((sock (usocket:socket-listen host port :reuse-address t :element-type '(unsigned-byte 8))))
    (values (bt:make-thread (lambda ()
                              (accept-connection sock)
                              (format *standard-output* "accept thread ended~%")))
            sock)))

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
        do (handle-connection sock1)))
;)   ; end eval-when
