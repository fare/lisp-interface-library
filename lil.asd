;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(in-package :asdf)

(defsystem :lil
  :description "LIL: Lisp Interface Library"
  :long-description "shortcut for the Lisp Interface Library"
  :class :package-system
  :defsystem-depends-on
  #.(unless (find-class :package-system nil) '(:asdf-package-system))
  :depends-on (:lisp-interface-library))

(defmethod perform ((op test-op) (system (eql (find-system :lil))))
  (asdf:test-system :lisp-interface-library))

(register-system-packages
 :closer-mop
 '(:c2mop
   :closer-common-lisp :c2cl
   :closer-common-lisp-user :c2cl-user))
