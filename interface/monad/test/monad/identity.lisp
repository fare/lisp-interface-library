(defpackage :lil/interface/monad/test/monad/identity
  (:use :cl
        :lil/interface/base
        :lil/interface/monad
        :lil/interface/monad/identity))
(in-package :interface/monad/test/monad/identity)

(defun test-identity-1 ()
  (eql 2
   (bind <identity> (result <identity> 1)
         (lambda (a) (result <identity> (+ a 1))))
   ;; => 2
   ))

(defun test-identity-2 ()
  (eql 16
   (bind <identity> (result <identity> 1)
         (lambda (a)
           (bind <identity> (result <identity> 15)
                 (lambda (b) (result <identity> (+ a b))))))
   ;; => 16
   ))

(defun test-identity-3 ()
  (eql 16
   (mlet* <identity>
       ((a (result 1))
        (b (result 15)))
     (result (+ a b)))
   ))

(defmethod check-invariant ((<i> <identity>) monad &key &allow-other-keys)
  (check-invariant <monad> monad)
    (loop for test in '(test-identity-1
                        test-identity-2
                        test-identity-3)
         :do (assert (eq T (funcall test))
                     NIL "~A is failing ~A" monad test)))
