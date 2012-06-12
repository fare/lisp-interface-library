;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :lisp-interface-library-test
  :description "Testing lisp-interface-library"
  :depends-on (:lisp-interface-library :hu.dwim.stefil :fare-utils :reader-interception)
  :components
  ((:module "test"
	    :components
	    ((:file "package")
	     (:file "functional-map" :depends-on ("package"))))))
