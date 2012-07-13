;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :pure-data-structures
  :description "Pure data structures using Interface-passing style"
  :long-description "Pure data structures using Interface-passing style"
  :depends-on (:pure-interfaces)
  :components
  (;;; IPS pure functional datastructures
   (:module "pure"
	    :components
	    ((:file "iterator")
             (:file "map")
             (:file "alist")
	     (:file "tree")
	     (:file "hash-table")
	     (:file "fmim")
	     (:file "encoded-key-map")))))
