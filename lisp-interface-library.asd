(in-package :asdf)

(defsystem :lisp-interface-library
  :description "Long name alias for lil"
  :depends-on (:lil)
  :in-order-to ((test-op (test-op :lil))))
