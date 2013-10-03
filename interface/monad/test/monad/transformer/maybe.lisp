(defpackage :lil/interface/monad/test/monad/transformer/maybe
  (:use :cl
        :lil/interface/base
        :lil/interface/monad
        :lil/interface/monad/maybe
        :lil/interface/monad/transformer/maybe
        :lil/interface/monad/transformer
        :lil/interface/monad/test/monad))
(in-package :lil/interface/monad/test/monad/transformer/maybe)

(defmethod test-for-check-monad-laws ((<mt> <maybe-transformer>) v)
  (test-for-check-monad-laws <maybe>
   (test-for-check-monad-laws (inner <mt>) v)))

(defmethod check-invariant ((<mt> <maybe-transformer>) monad &key &allow-other-keys)
  (check-invariant <monad> monad)
  (check-invariant (inner <mt>) monad))
