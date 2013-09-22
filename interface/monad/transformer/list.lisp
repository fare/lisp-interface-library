;;; Interface Passing Style : Monad : Transformer : List
(defpackage :interface/monad/transformer/list
  (:nicknames :drewc.org/ips/monad/transformer/list)
  (:use :cl :interface/monad
	:interface/monad/list
	:interface/monad/transformer
	:interface/monad/identity)
  (:export #:<list-transformer>))
(in-package :interface/monad/transformer/list)

(interface:define-interface <list-transformer>
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
