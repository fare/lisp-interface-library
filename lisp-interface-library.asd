(defsystem "lisp-interface-library"
  :description "Long name alias for lil"
  :version (:read-file-form "version.text")
  :depends-on ("lil")
  :in-order-to ((test-op (test-op "lil"))))
