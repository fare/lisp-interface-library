
(defpackage :interface/monad/test/monad/continuation
    (:use :cl 
          :interface/monad
          :interface/monad/continuation
          :interface/run)
    (:import-from :interface/monad/test/monad))
(in-package :interface/monad/test/monad/continuation)
  
(defmethod interface/monad/test/monad:test-for-check-monad-laws
    ((<c> <continuation>) cmv)
  (interface/monad/test/monad:test-for-check-monad-laws
   <monad> (run <c> cmv)))
                                                      
(defun test-continuation-1-fn ()
          (let* (k 
                 (mv (mlet* <continuation> 
                         ((a (result 1))
                          (b (call/cc (lambda (cc)
                                        (setf k cc)
                                        (result 2))))
                          (c (result 3)))
                       (result (list a b c
                                     (make-string (+ a b c) 
                                                  :initial-element #\a))))))
            (list (run <continuation> mv) k))
                                                                   
          ;; => ((1 2 3 "aaaaaa") #<CLOSURE (LAMBDA # :IN CALL/CC) {D0BE46D}>)
            
          ;; > (run <continuation> (funcall (second *) 7))
          ;; => (1 7 3 "aaaaaaaaaaa")
          ;; > (run <continuation> (funcall (second **) 0))
          ;; > (1 0 3 "aaaa")
          )

(defun test-continuation-1 ()
  (let* ((v (test-continuation-1-fn))
         (v1 (run <continuation> (funcall (second v) 7)))
         (v2 (run <continuation> (funcall (second v) 0))))
    (and (equalp (first v) '(1 2 3 "aaaaaa"))
         (equalp v1 '(1 7 3 "aaaaaaaaaaa"))
         (equalp v2 '(1 0 3 "aaaa"))
         T)))


    
(defmethod interface:check-invariant ((<c> <continuation>) monad &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (loop for test in '(test-continuation-1)
     :do (assert (eq T (funcall test))
                 NIL "~A is failing ~A" monad test)))
