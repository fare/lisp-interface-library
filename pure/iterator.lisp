;;;;; Pure Iterators

(uiop:define-package :lil/pure/iterator
  (:use :closer-common-lisp :core :lil/interface/base)
  (:use-reexport :lil/interface/iterator)
  (:export
   #:<fount> #:<sink>
   #:iterator #:next
   #:collector #:collect #:result
   #:flow
   #:<devnull>
   #:make-number-iterator))
(in-package :lil/pure/iterator)

(define-interface <fount> (<type>) () (:abstract))
(define-interface <sink> (<type>) () (:abstract))

(defgeneric iterator (<fount> fount)
  (:documentation "Given a <FOUNT> interface and an object FOUNT, return
an initial ITERATOR state with which to start iterating."))

(defgeneric next (<fount> iterator)
  (:documentation "Given a <FOUNT> interface and an ITERATOR state, return two or more values:
1- a boolean DATAP which is true if there is data, false if the end was reached;
2- the NEXT iterator state if any (can be NIL for some interfaces or if the end is reached);
3- zero, one or more VALUES produced by this iteration step."))

(defgeneric collector (<sink> sink)
  (:documentation "Given a <SINK> interface and a SINK object, return
an initial COLLECTOR state with which to start collecting."))

(defgeneric collect (<sink> collector &rest values)
  (:documentation "Given a <SINK> interface, a COLLECTOR state some VALUES, return
a new COLLECTOR state to which the values were added"))

(defgeneric result (<sink> collector)
  (:documentation "Given a <SINK> interface and a COLLECTOR, return
the final RESULT from the collecting process"))


(defgeneric flow (<fount> <sink> fount sink)
  (:documentation
   "Given <FOUNT> and <SINK> interfaces and FOUNT and SINK objects,
let data flow from the FOUNT to the SINK, and return the result"))
