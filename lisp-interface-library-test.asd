;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :lisp-interface-library-test
  :description "Testing lisp-interface-library"
  :depends-on (:lisp-interface-library :hu.dwim.stefil :fare-utils :reader-interception)
  :components
  ((:module "test"
	    :components
	    ((:file "package")
	     (:file "utilities" :depends-on ("package"))
	     (:file "pure-map" :depends-on ("utilities"))
	     (:file "stateful-map" :depends-on ("utilities"))))))
