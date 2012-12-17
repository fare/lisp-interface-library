
(defpackage :interface/monad/test/monad/transformer/list
    (:use :cl 
          :interface/monad
          :interface/monad/transformer/list
          :interface/monad/transformer)
    (:import-from :interface/monad/test/monad)
    (:import-from :interface/monad/test/monad/list))
(in-package :interface/monad/test/monad/transformer/list)
  
(defmethod interface/monad/test/monad:test-for-check-monad-laws
    ((<lt> <list-transformer>) mv)
  (interface/monad/test/monad:test-for-check-monad-laws
   (inner <lt>)
   (bind (inner <lt>) mv
         (lambda (v)
           (result (inner <lt>)
                   (interface/monad/test/monad:test-for-check-monad-laws
                    interface/monad/list:<list> v))))))

(defmethod interface:check-invariant ((<lt> <list-transformer>) monad 
                                      &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (interface:check-invariant (inner <lt>) monad))
