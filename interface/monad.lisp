;;; Interface Passing Style : Monad

(defpackage :lil/interface/monad
  (:documentation
   "This package is the <MONAD> interface.

It contains and exports:

<MONAD>
RESULT
BIND
FAIL
MLET*")

  (:use :cl)
  (:import-from :lil/interface/definition #:define-interface #:with-interface)
  (:import-from :lil/interface/base #:<type>)
  (:export
   #:<monad>
   #:result
   #:bind
   #:fail
   #:mlet*

   ;; The LIFT-FUNCTION is required for Interface Passing Style :
   ;; Monad : Transformer : List
   #:lift-function))

(in-package :lil/interface/monad)

(define-interface <monad> (<type>)
  ()
  (:singleton)
  (:documentation
   "An INTERFACE with three functions, RESULT, BIND and FAIL.

To be a 'proper' monad, it must also follow the Three Monad Laws.

CHECK-INVARIANT is available if the :INTERFACE/MONAD/TEST/MONAD is
loaded.")

  (:generic result (<monad> value)
    (:documentation
     "Takes a value of TYPE T and returns a monadic value that contains VALUE.

VALUE : anything
MONADIC-VALUE : The value of the type expected by the monad.

RESULT is the constructor of monad values."))

  (:generic bind (<monad> monadic-value monadic-function)
    (:documentation
     "Takes a monadic value, retrieves the contained value, and calls
the monadic function with it.

MONADIC-VALUE : The value of the type expected by the monad.
MONADIC-FUNCTION : a function that takes any VALUE and returns a
MONADIC-VALUE.

e.g., (BIND (RESULT 1) (LAMBDA (N) (RESULT (+ 1 N))))" ))

  (:generic fail (<monad>)))

(defmacro mlet* (monad bindings &body body)
  "The MLET* macro is like a LET* that is implemented with BIND, and a
WITH-INTERFACE so the proper functions are FLETd.

MONAD is a form that is passed to WITH-INTERFACE. BINDINGS are
essentially a list of variables that are expanded into a LAMBDA which
is passed to BIND. BODY is the body of the form.

e.g, (mlet* <list> ((x (list 1 2 3 4)))
      (result (1+ x)))

     ;; MACROEXPANDs to something like:

     ;; (WITH-INTERFACE (<LIST> <LIST>)
     ;;  (BIND (LIST 1 2 3 4) (LAMBDA (X) (RESULT (1+ X)))
"
  (destructuring-bind (monad &optional (functions-spec monad))
      (or (when (listp monad) monad) (list monad))
    `(macrolet ((%mlet* (((symbol monadic-value) &rest rest) &body body)
		  `(bind ,monadic-value
			 (lambda (,symbol)
			   ,@(when (string-equal (symbol-name symbol) "_")
				   `((declare (ignorable ,symbol))))
			   ,@(if rest
				 `((%mlet* ,rest
					   ,@body))
				 body)))))
       (with-interface (,monad ,functions-spec)
	 ,@(if bindings
	      `((%mlet* ,bindings ,@body))
	      body)))))

(defmethod lift-function ((<m> <monad>) function)
  "Take a function that accepts standard values and returns a standard
value, Turn it into a function that accepts monadic values and retuns
a monadic value
AKA: LiftM or MONAD-LIFT"
  (labels ((%mlift (function args &optional rargs)
	     (cond ((and (not rargs) (not args))
		    (result <m> (funcall function)))
		   ((not args)
		    (result <m>
			    (apply function (nreverse rargs))))
		   (t (bind <m> (first args)
			    (lambda (v)
			      (%mlift function (rest args)
				      (cons v rargs))))))))
  (lambda (&rest args)
    (%mlift function args))))
