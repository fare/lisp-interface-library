;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure Sets and Multisets

#+xcvb (module (:depends-on ("pure/map-interface")))

(in-package :pure)

(define-interface <set*> (<finite-collection> interface::<set*>) ()
  (:abstract))

(define-interface <set> (interface::<set> <set*>) ()
  (:abstract))

(define-interface <multiset> (interface::<multiset> <set*>) ()
  (:abstract)
  (:generic> increase-member-count (element count))
  (:generic> decrease-member-count (element count)))

(define-interface <set*-from-collection> (interface::<set*-from-collection> <set*>) ()
  (:method empty! ((i <set*-from-collection>) x) (empty (base-interface i))))

(define-interface <set-from-map> (<set> <set*-from-collection> interface::<set-from-map>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method singleton ((<i> <set-from-map>) element)
    (singleton (base-interface <i>) (cons element t))))

(define-interface <multiset-from-map> (<multiset> <set*-from-collection> interface::<multiset-from-map>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method singleton ((<i> <multiset-from-map>) element)
    (singleton (base-interface <i>) (cons element 1))))

(define-interface <set-from-multiset> (<set> <set*-from-collection> interface::<set-from-multiset>)
  ((base-interface :type <multiset>))
  (:parametric (base-interface)
    (make-interface :base-interface base-interface))
  (:method singleton ((<i> <set-from-multiset>) element)
    (singleton (base-interface <i>) element)))

(define-interface <multiset-from-set> (<multiset> <set*-from-collection> interface::<multiset-from-set>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method singleton ((<i> <set-from-multiset>) element)
    (singleton (base-interface <i>) element)))
