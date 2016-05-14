;;;;; Stateful types with empty objects

(uiop:define-package :lil/stateful/empty
  (:use :closer-common-lisp
        :lil/interface/definition)
  (:use-reexport
   :lil/interface/empty)
  (:export
   #:<empty!able> #:empty!))
(in-package :lil/stateful/empty)

(define-interface <empty!able> (<emptyable>) ()
  (:generic> empty! (map) (:in 1) (:values) (:out t)
   (:documentation "Clear the collection and make it empty. Return no value.")))
