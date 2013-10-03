(defpackage :lil/interface/monad/test/monad/transformer
  (:use :cl
        :lil/interface/monad
        :lil/interface/monad/transformer
        :lil/interface/monad/test/monad))
(in-package :lil/interface/monad/test/monad/transformer)

(defmethod test-for-check-monad-laws ((<t> <transformer>) v)
  (test-for-check-monad-laws (inner <t>) v))

(defmethod check-invariant ((<t> <transformer>) monad &key &allow-other-keys)
  (check-invariant <monad> monad)
  (check-invariant (inner <t>) monad))
