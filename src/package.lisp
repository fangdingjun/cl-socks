(defpackage :socks
  (:export
   :read-uint16
   :read-uint32
   :write-uint32
   :write-uint16
   :read-bytes-n
   :write-bytes-n
   :listen-for-connection
   :to-ip
   :connect
   :string-to-octets)
  (:use :cl))
