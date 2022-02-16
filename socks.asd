;;; vim: set ft=lisp:

(in-package :asdf)

(defsystem "socks"
  :class :package-inferred-system
  :serial t
  :version "0.0.1"
  :components ((:file "src/package")
               (:file "src/server")
               (:file "src/client"))
  :depends-on (:usocket :bordeaux-threads :flexi-streams)
  :description "socks proxy implement"
  :in-order-to (;(test-op (load-op "socks"))
                ;(test-op (load-op "socketserver"))
                (test-op (load-op "socks/test")))
  :perform (test-op (o c) (symbol-call :test/all :test-suite)))

(defsystem "socks/test"
  :components ((:file "test/all"))
  :depends-on (:usocket :socketserver :socks :fiveam))

;(register-system-packages "socks/src/app" '(:app)) 
;(register-system-packages "socks/test/all" '(:test/all))

#|
(setf asdf:*central-registry*
  (list #P"/home/dingjun/source/lisp/socks/" #P"/home/dingjun/quicklisp/quicklisp/"))
  (test/all:test-suite)
|#
