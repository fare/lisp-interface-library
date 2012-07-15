;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :pure-interfaces
  :description "Interfaces to pure data structures"
  :long-description "Abstract interfaces for pure data structures in interface-passing-style."
  :depends-on (:interface)
  :components
  ((:module "pure"
	    :components
	    ((:file "package")
	     (:file "iterator-interface" :depends-on ("package"))
	     (:file "map-interface" :depends-on ("package"
						 "iterator-interface"))
	     (:file "alist-interface" :depends-on ("package"))
	     (:file "tree-interface" :depends-on ("package"
						  "map-interface"))
	     (:file "hash-table-interface" :depends-on ("tree-interface"))
	     (:file "fmim-interface" :depends-on ("tree-interface"))
	     (:file "encoded-key-map-interface" :depends-on ("package"))))))
