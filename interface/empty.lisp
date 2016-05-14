;;;;; Empty

(uiop:define-package :lil/interface/empty
  (:use :closer-common-lisp
   :lil/interface/definition :lil/interface/base)
  (:export
   #:<emptyable> #:empty #:empty-p
   #:empty-object #:make-empty-object #:empty-object-p
   #:<empty-is-empty-object> #:<empty-is-nil>))
(in-package :lil/interface/empty)

(define-interface <emptyable> (<type>) ()
  (:abstract)
  (:generic> empty ()
   (:values object) (:out 0)
   (:documentation "Return an empty object of the emptyable type.
A constant one is pure, a new one if stateful."))
  (:generic> empty-p (object)
   (:in 1) (:values boolean)
   (:documentation "Return a boolean indicating whether the object is empty")))

(defclass empty-object () ())
(defun make-empty-object ()
  (make-instance 'empty-object))
(defun empty-object-p (x)
  (typep x 'empty-object))

(define-interface <empty-is-empty-object> (<emptyable>) ()
  (:abstract)
  (:method> check-invariant ((m empty-object) &key &allow-other-keys)
    (values))
  (:method> empty ()
    (make-empty-object))
  (:method> empty-p (object)
    (empty-object-p object)))

(define-interface <empty-is-nil> (<emptyable>) ()
  (:abstract)
  (:method> check-invariant ((m null) &key &allow-other-keys)
    (values))
  (:method> empty ()
    nil)
  (:method> empty-p (object)
    (null object)))

