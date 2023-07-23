;;; Interface Passing Style : Monad : Transformer : Maybe
(defpackage :lil/interface/monad/transformer/maybe
  (:use :cl :lil/interface/monad :lil/core)
  (:import-from :lil/interface/monad/identity
		#:<identity>)
  (:import-from :lil/interface/group
		#:<zero-plus> #:zero #:plus)
  (:import-from :lil/interface/empty
		#:empty #:empty-p)
  (:import-from :lil/interface/monad/maybe
		#:<maybe>
		#:fail
		#:maybe-or
		#:<maybe-empty>
		#:maybe.empty-interface)
  (:import-from :lil/interface/monad/transformer
		 #:<transformer>
		 #:inner
		 #:lift)
  (:export #:<maybe-transformer>))
(in-package :lil/interface/monad/transformer/maybe)

(define-interface <maybe-transformer>
    (<maybe> <transformer>)
  ()
  (:parametric (&optional (inner <identity>)
			  (empty-interface <maybe-empty>))
    (make-interface :inner inner :empty-interface empty-interface))
  (:singleton)
  (:method result ((<m> <maybe-transformer>) value)
	    (result (inner <m>) value))
  (:method bind ((<m> <maybe-transformer>) mv mf)
	   (bind (inner <m>) mv
		 (lambda (v)
		   (if (empty-p (maybe.empty-interface <m>) v)
		       mv
		       (funcall mf v)))))
  (:method zero ((<m> <maybe-transformer>))
	   (result (inner <m>) (call-next-method)))
  (:method plus ((<m> <maybe-transformer>) a b)
     (bind (inner <m>) a
	   (lambda (v)
	     (call-next-method <m> v b)))))
