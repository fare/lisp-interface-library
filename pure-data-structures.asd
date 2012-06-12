;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :pure-data-structures
  :description "Basic functions and macros, interfaces, pure and stateful datastructures"
  :long-description "fare-utilities is a small collection of utilities.
It contains a lot of basic everyday functions and macros,
but also a library of pure and stateful datastructures,
and Lisp extensions for memoization and reader interception."
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
