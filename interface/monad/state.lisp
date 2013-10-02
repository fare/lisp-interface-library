;;; Interface Passing Style : Monad : State
(defpackage :lil/interface/monad/state
  (:nicknames :drewc.org/ips/monad/state)
  (:use :cl :lil/interface/monad :lil/interface/definition)
  (:import-from :lil/interface/run
		#:<run>
		#:run)
  (:export #:<state>
	   #:update
	   #:fetch
	   #:put))
(in-package :lil/interface/monad/state)

(define-interface <state> (<monad> <run>)
  ()
  (:singleton)
  (:generic> update (<state> function)
    (:documentation "The UPDATE function takes the state and returns
it.  It also calls the THING with the current state and makes the
result into the new state."))
  (:generic fetch (<state>))
  (:generic put (<state> new-state))

  (:method> result (value)
    (lambda (state)
      (cons value state)))

  (:method> bind (mv mf)
   (lambda (context)
     (destructuring-bind (result . new-context)
	 (funcall mv context)
       (funcall (funcall mf result) new-context))))

  (:method> run (state-monadic-value &rest state-and-?)
	    (funcall state-monadic-value (first state-and-?)))

  (:method> update (function)
    (lambda (s)
      (cons s (funcall function s))))

  (:method> fetch ()
    (update #'identity))

  (:method> put (state)
    (update (constantly state))))

