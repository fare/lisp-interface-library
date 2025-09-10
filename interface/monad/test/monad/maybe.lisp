(defpackage :lil/interface/monad/test/monad/maybe
  (:use :cl
        :lil/core
        :lil/interface/base
        :lil/interface/empty
        :lil/interface/monad
        :lil/interface/monad/maybe
        :lil/interface/monad/test/monad))

(in-package :lil/interface/monad/test/monad/maybe)

(defun test-maybe-1 ()
  (let ((maybes (loop :repeat 10 
                   :collect 
                   (mlet* <maybe> ((a (result (oddp (random 10))))
                                   (b (or (and a 
                                               (result 'w00t))
                                          (fail))))
                     (result b)))))
    (and (position NIL maybes)
         (find 'w00t maybes)
         T)))

(defun test-maybe-2 ()
  (let ((res
         (let ((<m> (<maybe> <empty-is-empty-object>)))
           (mlet* (<m> <maybe>) 
               ((a (result (oddp (random 10))))
                (_ (fail)))
             (result a)))
         ;; => #<INTERFACE:EMPTY-OBJECT {C1CE539}>
          ))
    (and (empty-object-p res) t)))

(defun doit (&optional (<m> <maybe>)) 
  (flet ((mv ()
           (mlet* (<m> <maybe>) 
               ((a (result (random 1.0)))
                (b (result (< 0.5 a))))
             (if b 
                 (result a)
                 (fail)))))
    (mlet* (<m> <maybe>) ()
      (maybe-or (mv) (result 'w00t)))))

(defun test-maybe-3 ()
  (loop :for res 
     :in (loop :repeat 10 
            :collect (doit))
     :if (and (not (eq 'w00t res))
              (not (numberp res)))
     :do (return nil)
     :finally (return t)))
  
(defmethod check-invariant ((<m> <maybe>) monad &key &allow-other-keys)
  (check-invariant <monad> monad)
  (loop for test in '(test-maybe-1
                      test-maybe-2
                      test-maybe-3)
     :do (assert (eq T (funcall test)) 
                 NIL "~A is failing ~A" monad test)))
