
(defpackage :interface/monad/test/monad/maybe
  (:use :cl 
        :interface/monad
        :interface/monad/maybe)
  (:import-from :interface/monad/test/monad))

(in-package :interface/monad/test/monad/maybe)

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

  
(interface:define-interface <empty-is-empty-object>
    (interface:<empty-is-empty-object>) ()
    (:singleton))

(defun test-maybe-2 ()
  (let ((res
         (let ((<m> (<maybe> <empty-is-empty-object>)))
           (mlet* (<m> <maybe>) 
               ((a (result (oddp (random 10))))
                (_ (fail)))
             (result a)))
         ;; => #<INTERFACE:EMPTY-OBJECT {C1CE539}>
          ))
    (and (interface:empty-object-p res) t)))

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
  
(defmethod interface:check-invariant ((<m> <maybe>) monad &key &allow-other-keys)
  (interface:check-invariant <monad> monad)
  (loop for test in '(test-maybe-1
                      test-maybe-2
                      test-maybe-3)
     :do (assert (eq T (funcall test)) 
                 NIL "~A is failing ~A" monad test)))
