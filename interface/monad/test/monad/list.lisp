(defpackage :lil/interface/monad/test/monad/list
  (:use :cl
        :lil/interface/base
        :lil/interface/monad
        :lil/interface/monad/list
        :lil/interface/monad/test/monad))

(in-package :lil/interface/monad/test/monad/list)

(defmethod test-for-check-monad-laws ((<l> <list>) list)
  (test-for-check-monad-laws <monad> (first list)))

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

(defmethod check-invariant ((<l> <list>) monad &key &allow-other-keys)
  (check-invariant <monad> monad)
  (loop for test in '(test-list-1
                      test-list-2
                      test-list-3
                      )
     :do (assert (eq T (funcall test))
                 NIL "~A is failing ~A" monad test)))
