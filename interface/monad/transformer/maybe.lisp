;;; Interface Passing Style : Monad : Transformer : Maybe
(defpackage :interface/monad/transformer/maybe
  (:nicknames :drewc.org/ips/monad/transformer/maybe)
  (:use :cl :interface/monad)
  (:import-from :interface/monad/identity
		#:<identity>)
  (:import-from :interface/zero-plus
		#:zero
		#:plus)
  (:import-from :interface
		#:empty
		#:empty-p)
  (:import-from :interface/monad/maybe
		#:<maybe>
		#:fail
		#:maybe-or
		#:<maybe-empty>
		#:maybe.empty-interface)
  (:import-from :interface/monad/transformer
		 #:<transformer>
		 #:inner
		 #:lift)
  (:export #:<maybe-transformer>))
(in-package :interface/monad/transformer/maybe)

(interface:define-interface <maybe-transformer>
    (<maybe> <transformer>)
  ()
  (:parametric (&optional (inner <identity>)
			  (empty-interface <maybe-empty>))
    (make-instance '<maybe-transformer>
		   :inner inner
		   :empty-interface empty-interface))
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
