;;;;; Read-only interfaces common to pure and stateful collections

(uiop:define-package :lil/interface/sequence
  (:use
   :closer-common-lisp
   :lil/core
   :lil/interface/base
   :lil/interface/collection)
  (:use-reexport
   :lil/interface/empty
   :lil/interface/size
   :lil/interface/fold
   :lil/interface/iterator)
  (:export
   #:<sequence> ;; to be shadowed by pure and stateful packages.
   #:sequence-list #:list-sequence))

(in-package :lil/interface/sequence)

;; TODO: create this interface...
(define-interface <sequence> (<finite-collection>) ()
  (:abstract))
