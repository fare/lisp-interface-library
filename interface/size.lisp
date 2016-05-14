;;;;; Size
(uiop:define-package :lil/interface/size
  (:use :closer-common-lisp :uiop
        :lil/core :lil/interface/base)
  (:export
   #:<sizable> #:size #:size<=n-p
   #:<sizable-size<=n-p-from-size>))
(in-package :lil/interface/size)

;;; Size
(define-interface <sizable> (<type>) ()
  (:abstract)
  (:generic> size (object) (:in 1) (:values size) (:out nil)
   (:documentation "Size the object, e.g. number of elements in a collection"))
  (:generic> size<=n-p (object n) (:in 1) (:values boolean) (:out nil)
   (:documentation "Is the size of the object less or equal to integer n?")))

(define-interface <sizable-size<=n-p-from-size> (<sizable>) ()
  (:abstract)
  (:method> size<=n-p (map n)
    (<= (size map) n)))

