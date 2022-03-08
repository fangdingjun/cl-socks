;;; vim: set ft=lisp:

(in-package :asdf)

(defsystem "socks"
  :serial t
  :version "0.0.1"
  :components ((:file "src/package")
               (:file "src/server")
               (:file "src/socks5")
               (:file "src/socks4")
               (:file "src/client"))
  :depends-on (:usocket :bordeaux-threads :flexi-streams)
  :description "socks proxy implement"
  :in-order-to ((test-op (load-op "socks/test")))
  :perform (test-op (o c) (symbol-call :test/all :test-suite)))

(defsystem "socks/test"
  :components ((:file "test/all"))
  :depends-on (:usocket :socketserver :socks :fiveam))
