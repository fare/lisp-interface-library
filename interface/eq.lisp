;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Equality

#+xcvb (module (:depends-on ("interface/interface")))

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

(define-interface <eq> () ())
(defparameter <eq> (fmemo:memoized-funcall 'make-instance '<eq>))
(defgeneric == (i x y))
(defgeneric test-function (i)
  (:documentation "test function for <eq> interface"))

(defmethod == ((i <eq>) x y)
  (eql x y))
(defmethod test-function ((i <eq>))
  #'eql)

(define-interface <eq-simple> (<eq>) ())
(defmethod test-function ((i <eq-simple>))
  #'(lambda (x y) (== i x y)))

(define-interface <eq-slot> (<eq>)
  ((test :initform #'eql :initarg :test :reader test-function)))
(defmethod == ((i <eq-slot>) x y)
  (funcall (test-function i) x y))

(define-interface <hashable> (<eq>) ())
(defgeneric hash (i x))
(defmethod hash ((i <hashable>) x)
  (sxhash x))

(define-interface <equal> (<hashable>) () (:singleton))
(defmethod == ((i <equal>) x y)
  (equal x y))
(defmethod test-function ((i <equal>))
  #'equal)
