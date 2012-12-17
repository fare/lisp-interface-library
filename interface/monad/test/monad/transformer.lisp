
(defpackage :interface/monad/test/monad/transformer
    (:use :cl 
          :interface/monad
          :interface/monad/transformer)
    (:import-from :interface/monad/test/monad))
(in-package :interface/monad/test/monad/transformer)
  
(defmethod interface/monad/test/monad:test-for-check-monad-laws
    ((<t> <transformer>) v)
  (interface/monad/test/monad:test-for-check-monad-laws
   (inner <t>) v))
                                                      
(defmethod interface:check-invariant ((<t> <transformer>) monad &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (interface:check-invariant (inner <t>) monad))
