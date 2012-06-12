;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :lisp-interface-library-test
  :description "Testing lisp-interface-library"
  :depends-on (:lisp-interface-library :hu.dwim.stefil)
  :components
  ((:file "package")
   (:file "functional-map" :depends-on ("package"))))
