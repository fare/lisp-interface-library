;;; Interface Passing Style : Monad : Continuation

(defpackage :interface/monad/continuation
  (:nicknames :drewc.org/ips/monad/continuation)
  (:use :cl :interface/monad)
  (:import-from :interface/run
		#:<run>
		#:run)
  (:export #:<continuation>
	   #:call/cc))

(in-package :drewc.org/ips/monad/continuation)

(interface:define-interface <continuation> (<monad> <run>)
  ()
  (:singleton)
  (:generic call/cc (<continuation> function))
  (:method> result (value)
   (lambda (k) (funcall k value)))
  (:method> bind (mv mf)
   ;; m >>= f  = Cont (\k -> runCont m (\a -> runCont (f a) k))
   (lambda (k)
      (funcall mv (lambda (a) (funcall (funcall mf a) k)))))
  (:method> call/cc (fn)
   ;; callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
   (lambda (k)
     (funcall 
      (funcall fn (lambda (a)
		    (lambda (_)
		      (declare (ignore _))
		      (funcall k a))))
	     k)))
  (:method> run (mv &rest args)
    (declare (ignore args))
   (funcall mv #'identity)))


