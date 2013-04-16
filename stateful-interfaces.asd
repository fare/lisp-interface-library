;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :stateful-interfaces
  :description "Detached interfaces to stateful datastructures"
  :long-description "Abstract detached interfaces for stateful datastructures in interface-passing-style."
  :depends-on (:interface)
  :components
  ((:module "stateful"
	    :components
	    ((:file "package")
	     (:file "iterator-interface" :depends-on ("package"))
	     (:file "collection" :depends-on ("iterator-interface"))
	     (:file "map-interface" :depends-on ("collection"))
	     (:file "tree-interface" :depends-on ("map-interface"))
	     (:file "hash-table-interface" :depends-on ("tree-interface"))
	     (:file "encoded-key-map-interface" :depends-on ("map-interface"))
	     (:file "queue-interface" :depends-on ("collection"))))))
