(defpackage :lil/interface/monad/test/monad/transformer/list
  (:use :cl
        :lil/interface/base
        :lil/interface/monad
        :lil/interface/monad/list
        :lil/interface/monad/transformer/list
        :lil/interface/monad/transformer
        :lil/interface/monad/test/monad
        :lil/interface/monad/test/monad/list))
(in-package :lil/interface/monad/test/monad/transformer/list)

(defmethod test-for-check-monad-laws ((<lt> <list-transformer>) mv)
  (test-for-check-monad-laws
   (inner <lt>)
   (bind (inner <lt>) mv
         (lambda (v)
           (result (inner <lt>)
                   (test-for-check-monad-laws <list> v))))))

(defmethod check-invariant ((<lt> <list-transformer>) monad
                                      &key &allow-other-keys)
  (check-invariant <monad> monad)
  (check-invariant (inner <lt>) monad))
