;;;;; Order

(uiop:define-package :lil/pure/lexicographic
  (:use :closer-common-lisp
   :lil/core :lil/interface/base
   :lil/interface/eq :lil/pure/sequence :lil/interface/order)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:<lexicographic>))

(in-package :lil/pure/lexicographic)

(define-interface <lexicographic> (<sequence> <order-from-compare>)
  ((element-interface :type <order> :reader element-interface))
  (:abstract)
  (:method> compare (x y)
    (nest
     (let ((x-iter (iterator <lexicographic> x))
           (y-iter (iterator <lexicographic> y))))
     (loop)
     (multiple-value-bind (x-datap x-next x-item) (next <lexicographic> x-iter))
     (multiple-value-bind (y-datap y-next y-item) (next <lexicographic> y-iter))
     (if (not x-datap) (return (if y-datap -1 0)))
     (if (not y-datap) (return 1))
     (let ((comp (compare (element-interface <lexicographic>) x-item y-item))))
     (ecase comp
       ((-1 1) (return comp))
       ((0) (setf x-iter x-next y-iter y-next))))))
