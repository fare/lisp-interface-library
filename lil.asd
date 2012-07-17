;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :lil
  :description "LIL: Lisp Interface Library"
  :long-description "shortcut for the Lisp Interface Library"
  :depends-on (:lisp-interface-library))

(defmethod perform ((op test-op) (system (eql (find-system :lisp-interface-library))))
  (asdf:test-system :lisp-interface-library))
