
(defpackage :interface/monad/test/monad/transformer/maybe
    (:use :cl 
          :interface/monad
          :interface/monad/transformer/maybe
          :interface/monad/transformer)
    
    (:import-from :interface/monad/test/monad))
(in-package :interface/monad/test/monad/transformer/maybe)
  
(defmethod interface/monad/test/monad:test-for-check-monad-laws
    ((<mt> <maybe-transformer>) v)
  (interface/monad/test/monad:test-for-check-monad-laws
   interface/monad/maybe:<maybe> 
   (interface/monad/test/monad:test-for-check-monad-laws
    (inner <mt>) v)))
                                                      
(defmethod interface:check-invariant ((<mt> <maybe-transformer>) monad &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (interface:check-invariant (inner <mt>) monad))
