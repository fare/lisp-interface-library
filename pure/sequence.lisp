;;;;; Read-only interfaces common to pure and stateful collections

(uiop:define-package :lil/pure/sequence
  (:use
   :closer-common-lisp
   :core
   :lil/interface/base)
  (:use-reexport
   :lil/pure/empty
   :lil/interface/empty
   :lil/interface/size
   :lil/interface/fold
   :lil/pure/iterator
   :lil/pure/collection
   :lil/interface/sequence)
  (:shadow
   #:<sequence>)) ;; note: shadowed in pure, stateful

(in-package :lil/pure/sequence)

;; TODO: create this interface...
(define-interface <sequence> (interface::<sequence> <finite-collection>) ()
  (:abstract))
