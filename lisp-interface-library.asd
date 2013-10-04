(in-package :asdf)

(defsystem :lisp-interface-library
  :depends-on (:lil)
  :in-order-to (test-op (test-op :lil)))
