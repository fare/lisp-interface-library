;;; Interface Passing Style : Monad : Transformer
(defpackage :lil/interface/monad/transformer
  (:use :cl :lil/interface/monad :lil/interface/definition)
  (:import-from :lil/interface/monad/identity
		#:<identity>)
  (:export 
   #:<transformer>
   #:inner
   #:lift))
(in-package :lil/interface/monad/transformer)

(define-interface <transformer> (<monad>)
  ((inner-monad :accessor inner
		:initarg :inner
		:initarg inner
		:initform <identity>))
  (:singleton)
  (:generic> lift (inner-monadic-value))
  (:parametric (&optional (inner <identity>))
	       (make-instance '<transformer> :inner inner))
  (:method> lift (inner-monadic-value)
	    inner-monadic-value)
  (:method result ((<m> <transformer>) v)
	   (lift <m> (result (inner <m>) v)))
  (:method bind ((<m> <transformer>) mv mf)
	   (lift <m> (bind (inner <m>) mv mf)))
  (:method fail ((<m> <transformer>))
	   (lift <m> (fail (inner <m>)))))

(defmethod print-object ((object <transformer>) stream)
  (print-unreadable-object (object stream :type t)
    (princ (inner object) stream)))

