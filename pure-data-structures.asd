;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :pure-data-structures
  :description "Pure data structures using Interface-passing style"
  :long-description "Pure data structures using Interface-passing style"
  :depends-on (:pure-interfaces)
  :components
  (;;; IPS pure functional datastructures
   (:module "pure"
	    :components
	    ((:file "alist")
	     (:file "tree" :depends-on ("alist"))
	     (:file "hash-table" :depends-on ("tree"))
	     (:file "fmim" :depends-on ("tree"))
	     (:file "encoded-key-map")))))
