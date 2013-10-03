;;; Interface Passing Style : Monad : Transformer : List
(defpackage :lil/interface/monad/transformer/list
  (:use :cl
        :lil/interface/definition
        :lil/interface/monad
	:lil/interface/monad/list
	:lil/interface/monad/transformer
	:lil/interface/monad/identity)
  (:export #:<list-transformer>))
(in-package :lil/interface/monad/transformer/list)

(define-interface <list-transformer>
    (<list> <transformer>)
  ()
  (:singleton)
  (:parametric (&optional (inner <identity>))
    (make-instance '<list-transformer> :inner inner))
  (:method result ((m <list-transformer>) value)
	   (result (inner m)
		   (list value))))

(defmethod lift ((m <list-transformer>) value)
  (list value))

(defmethod bind ((m <list-transformer>) mv mf)
  (bind (inner m)
	mv
	(lambda (list)
	  (reduce
	   (lift-function (inner m) #'append)
	   (loop :for v :in list
	      :collect (funcall mf v))))))
