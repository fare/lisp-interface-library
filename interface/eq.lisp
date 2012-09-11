;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Equality

#+xcvb (module (:depends-on ("interface/base")))

(in-package :cl)

(defpackage :eq
  (:use :cl :interface)
  (:export
   #:<eq> #:<eq-simple> #:<eq-slot>
   #:<equal>
   #:== #:test-function
   #:<hashable>
   #:hash
   ))

(in-package :eq)

(define-interface <eq> (<type>)
  ()
  (:singleton)
  (:generic == (i x y) (:in 1 2) (:values bool))
  (:generic
   test-function (i) (:values fun)
   (:documentation "test function for <eq> interface")))

(defmethod == ((i <eq>) x y)
  (eql x y))
(defmethod test-function ((i <eq>))
  #'eql)

(define-interface <eq-simple> (<eq>) () (:singleton))
(defmethod test-function ((i <eq-simple>))
  #'(lambda (x y) (== i x y)))

(define-interface <eq-slot> (<eq>)
  ((test :initform #'eql :initarg :test :reader test-function))
  (:parametric (&key test) (make-interface :test test)))
(defmethod == ((i <eq-slot>) x y)
  (funcall (test-function i) x y))

(define-interface <hashable> (<eq>)
  ()
  (:generic hash (i x) (:in 1) (:values bool)))

(defmethod hash ((i <hashable>) x)
  (sxhash x)) ; Note: matches equal, not eql

(define-interface <equal> (<hashable>) () (:singleton))
(defmethod == ((i <equal>) x y)
  (equal x y))
(defmethod test-function ((i <equal>))
  #'equal)
