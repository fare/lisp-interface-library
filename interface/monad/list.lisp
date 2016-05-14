;;; Interface Passing Style : Monad : List

(defpackage :lil/interface/monad/list
  (:use :cl :lil/interface/monad :core)
  (:export #:<list>))

(in-package :lil/interface/monad/list)

(define-interface <list> (<monad>)
  ()
  (:singleton)
  (:method> result (value) (list value))
  (:method> bind (mvs mf)
	    (loop :for mv in mvs
		  :append (funcall mf mv))))

