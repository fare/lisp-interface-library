;;; Interface Passing Style : Monad : Test : Monad

(defpackage :interface/monad/test/monad
  (:use)
  (:import-from :interface/monad)
  (:export
   #:check-monad-left-identity
   #:check-monad-right-identity
   #:check-monad-associativity
   #:check-monad-laws
   #:test-for-check-monad-laws))

(in-package :interface/monad)

(defun interface/monad/test/monad:check-monad-left-identity
    (<m>)
  "Left Identity: '(BIND (RESULT X) MF) = (FUNCALL MF X)' MLET* style"
  (mlet* (<m> <monad>)
      ((monadic-function (result (lambda (x) (result (1+ x)))))
       (a (bind (result 1) monadic-function))
       (b (funcall monadic-function 1)))
    (result (= a b))))

(defun interface/monad/test/monad:check-monad-right-identity
    (<m>)
  "Right Identity: '(BIND MV RESULT) = MV' done MLET* style"
  (mlet* (<m> <monad>)
      ((a (bind (result 1) #'result))
       (b (result 1)))
    (result (= a b))))

(defun interface/monad/test/monad:check-monad-associativity
    (<m>)
  "Associativity: '(BIND (BIND MV MF) MF2)
                       = (BIND MV (LAMBDA (X) (BIND (MF X) MF2)))'"
  (mlet* (<m> <monad>)
      ((mv (result (result 1)))
       (mf (result (lambda (i) (result (1+ i)))))
       (mf2 (result (lambda (i) (result (format nil "foo-~A" i)))))
       (a (bind (bind MV MF) MF2))
       (b (bind MV (lambda (x) (bind (funcall MF x) MF2)))))
    (result (equalp a b))))

(defun interface/monad/test/monad:check-monad-laws (monad)
  "This checks the three monad laws using :
CHECK-MONAD-LEFT-IDENTITY
CHECK-MONAD-RIGHT-IDENTITY
CHECK-MONAD-ASSOCIATIVITY"
  (mlet* (monad <monad>)
      ((left
	(interface/monad/test/monad:check-monad-left-identity monad))
       (right
	(interface/monad/test/monad:check-monad-right-identity monad))
       (associativity
	(interface/monad/test/monad:check-monad-associativity monad)))
    (let ((results (list left right associativity)))
      (result (null
	       (assert (every #'(lambda (r) (eq t r)) results)
		       nil
		       "The monad ~A does not pass the three monad laws.

Left: ~A
Right: ~A
Associativity: ~A"
		monad left right associativity))))))

(defmethod interface/monad/test/monad:test-for-check-monad-laws
    ((<m> <monad>) monadic-value)
  (eq T monadic-value))

(defmethod interface:check-invariant ((<m> <monad>) (monad <monad>)
			    &key &allow-other-keys)
  (let ((mv (interface/monad/test/monad:check-monad-laws monad)))
    (assert (interface/monad/test/monad:test-for-check-monad-laws
	     monad mv)
	    NIL "The TEST-FOR-CHECK-MONAD-LAWS is failing for ~A : ~A is the value"
	    monad mv)))

