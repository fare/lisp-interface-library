
(defpackage :interface/monad/test/monad/state
    (:use :cl 
          :interface/monad
          :interface/monad/state
          :interface/run)
    (:import-from :interface/monad/test/monad))
(in-package :interface/monad/test/monad/state)
  
(defmethod interface/monad/test/monad:test-for-check-monad-laws
    ((<s> <state>) smv)
  (interface/monad/test/monad:test-for-check-monad-laws
   <monad> (first (run <s> smv))))
                                                      
(defun test-state-1 ()
  (equalp '(THIS-IS-THE-CURRENT-VALUE . THIS-IS-THE-CURRENT-STATE)
          (let ((MV (result <state> 'this-is-the-current-value)))
            (run <state> MV 'this-is-the-current-state))
          ;; => (THIS-IS-THE-CURRENT-VALUE . THIS-IS-THE-CURRENT-STATE)
          ))

(defun test-state-2 ()
  (equalp '((LIST) THIS IS A LIST)
          (let ((MV (update <state> 
                            (lambda (s)
                              (list* 'this 'is 'a s)))))
            (run <state> MV '(list)))
          ;; => ((LIST) THIS IS A LIST)
          ))

(defun test-state-3 ()
  (equalp '((PREVIOUS LIST) THIS IS A PREVIOUS LIST)
          (let ((MV (mlet* <state> 
                        ((prev (fetch))
                         (_ (put (list* 'this 'is 'a prev))))
                      (result prev))))
            (run <state> MV '(previous list)))
          ;; => ((PREVIOUS LIST) THIS IS A PREVIOUS LIST)
          ))
    
(defmethod interface:check-invariant ((<s> <state>) monad &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (loop for test in '(test-state-1
                      test-state-2
                      test-state-3)
     :do (assert (eq T (funcall test))
                 NIL "~A is failing ~A" monad test)))
