;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; stateful iterator

#+xcvb (module (:depends-on ("interface/iterator" "stateful/package")))

(in-package :stateful)

(define-interface <fount> (<interface>) ())
(define-interface <sink> (<interface>) ())

(define-condition end-of-iteration (error) ())

(defgeneric iterator (<fount> fount)
  (:documentation "Given a <FOUNT> interface and an object FOUNT,
return an initial ITERATOR state with which to start iterating."))

(defgeneric next (<fount> iterator)
  (:documentation "Given a <FOUNT> interface and an ITERATOR state,
raise an END-OF-ITERATION condition if the iterator has reached its end,
otherwise, side-effect the iterator to go to its next state and
return values produced by this iteration step."))

(defgeneric collector (<sink> sink)
  (:documentation "Given a <SINK> interface and a SINK object, return
an initial COLLECTOR state with which to start collecting."))

(defgeneric collect (<sink> collector &rest values)
  (:documentation "Given a <SINK> interface, a COLLECTOR state some VALUES,
return no values."))

(defgeneric result (<sink> collector)
  (:documentation "Given a <SINK> interface and a COLLECTOR, return
the final RESULT from the collecting process"))

(defgeneric flow (<fount> <sink> fount sink)
  (:documentation
   "Given <FOUNT> and <SINK> interfaces and FOUNT and SINK objects,
let data flow from the FOUNT to the SINK, and return the result"))

(define-interface <for-each> (<sink>) () (:singleton))
