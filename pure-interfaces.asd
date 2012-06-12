;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :pure-interfaces
  :description "Interfaces to pure data structures"
  :long-description "Abstract interfaces for pure data structures in interface-passing-style."
  :depends-on (:interface)
  :components
  ((:module "pure"
	    :components
	    ((:file "package")
	     (:file "map" :depends-on ("package"))))))
