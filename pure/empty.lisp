;;;;; Pure types with empty objects

(uiop:define-package :lil/pure/empty
  (:use :closer-common-lisp
        :lil/interface/definition)
  (:use-reexport
   :lil/interface/empty)
  (:export
   #:<empty!able> #:empty!))
(in-package :lil/pure/empty)

(define-interface <empty!able> (<emptyable>) ()
  (:generic> empty! (ignored) (:in 1)
    (:values empty) (:out 0)
    (:method> (ignored)
       (declare (ignorable ignored))
       (empty))
    (:documentation
     "This function is pretty useless to call, but allows for
      automatic generation of mutating interface wrappers.")))
