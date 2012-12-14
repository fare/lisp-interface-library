
(defpackage :interface/monad/test/monad/list
    (:use :cl 
          :interface/monad
          :interface/monad/list)
    (:import-from :interface/monad/test/monad))
  
(in-package :interface/monad/test/monad/list)
  
(defmethod interface/monad/test/monad:test-for-check-monad-laws 
    ((<l> <list>) list)
  (interface/monad/test/monad:test-for-check-monad-laws 
   <monad> (first list)))
                                                      
(defun test-list-1 () 
  (equalp '(1) 
          (mlet* <list> () (result 1)) ;; => (1)
          ))

(defun test-list-2 () 
  (equalp '(2 3 4 5)
          (mlet* <list> () 
            (bind (list 1 2 3 4) 
                  (lambda (x) (result (1+ x)))))
          ;; => (2 3 4 5)
          ))

(defun test-list-3 () 
  (equalp '(2 4 6 8)
          (mlet* <list> ((who (list 1 3 5 7))
                         (do (list who (1+ who)))
                         (we (unless (oddp do) 
                               (list do))))
            (result we))
          ;; => (2 4 6 8)
          ))
    
(defmethod interface:check-invariant ((<l> <list>) monad &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (loop for test in '(test-list-1
                      test-list-2
                      test-list-3
                      )
     :do (assert (eq T (funcall test)) 
                 NIL "~A is failing ~A" monad test)))
