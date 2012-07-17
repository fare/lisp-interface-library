;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :stateful-datastructures
  :description "Stateful datastructures using Interface-passing style"
  :long-description "Stateful datastructures using Interface-passing style"
  :depends-on (:stateful-interfaces :pure-interfaces)
  :components
  (;;; IPS stateful datastructures
   (:module "stateful"
	    :components
	    ((:file "iterator")
             (:file "map")
	     (:file "tree")
	     (:file "hash-table")
	     (:file "encoded-key-map")))))
