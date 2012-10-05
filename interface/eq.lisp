;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Equality

#+xcvb (module (:depends-on ("interface/base")))

(in-package :interface)

(define-interface <eq> (<type>) ()
  (:abstract)
  (:generic == (<eq> x y) (:in 1 2) (:values bool))
  (:generic eq-function (i) (:values fun)
   (:documentation "test function for <eq> interface")))

(define-interface <eql> (<eq>) ()
  (:singleton)
  (:method == (x y)
    (eql x y))
  (:method eq-function ()
    #'eql))

(define-interface <eq-from-==> (<eq>) ()
  (:abstract)
  (:method eq-function ()
    #'(lambda (x y) (== x y))))

(define-interface <eq-from-eq-function> (<eq>)
  ((eq-function :initarg :eq-function :reader eq-function))
  (:parametric (&key eq-function) (make-interface :eq-function eq-function))
  (:method == (x y)
    (funcall (eq-function) x y)))

(define-interface <hashable> (<eq>) ()
  (:abstract)
  (:generic hash (<hashable> x) (:in 1) (:values bool)))

(define-interface <equal> (<hashable>) () (:singleton)
  (:method == (x y)
    (equal x y))
  (:method eq-function ()
    #'equal)
  (:method hash (x)
    (sxhash x)))
