;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :pure-interfaces
  :description "LIL: abstract interfaces for pure data structures"
  :long-description "LIL is a collection of mostly pure data structures in interface-passing-style.
See http://fare.livejournal.com/155094.html regarding interface-passing-style."
  :depends-on (:interface)
  :components
  ((:module "pure"
	    :components
	    ((:file "package")
	     (:file "map" :depends-on ("package"))))))
