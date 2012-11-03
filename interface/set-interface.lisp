;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; mapping of keys to values -- part common to pure and stateful

#+xcvb (module (:depends-on ("interface/base")))

(in-package :interface)

(define-interface <set*> (<finite-collection>) ()
  (:abstract)
  (:method> value-interface () <boolean>)
  (:generic> member-p (set* element) (:in 1) (:values foundp)
   (:documentation "Is a given object member of the set?"))
  (:generic> member-count (set* element) (:in 1) (:values foundp)
   (:documentation "How many times for the element appear in the set*?"))
  (:generic> set-list (set) (:in 1) (:values list)
   (:documentation "Convert a set of given interface to a list"))
  (:generic> list-set (list) (:values map) (:out 0)
   (:documentation "Convert a list to a set of given interface")))

(define-interface <set> (<set*> <map>) () ;; a set is a map from the key interface to boolean
  (:abstract)
  (:method> value-interface () <boolean>))

(define-interface <multiset> (<set*> <map>) () ;; a multiset is a map from the key interface to (positive) integer
  (:abstract)
  (:method> value-interface () <integer>)
  (:generic> multiset-list (set) (:in 1) (:values list)
   (:documentation "Convert a multiset of given interface to a list"))
  (:generic> list-multiset (list) (:values map) (:out 0)
   (:documentation "Convert a list to a multiset of given interface")))
