;;; Interface Passing Style : Monad : List

(defpackage :lil/interface/monad/list
  (:nicknames :drewc.org/ips/monad/list)
  (:use :cl :lil/interface/monad :lil/interface/definition)
  (:export #:<list>))

(in-package :lil/interface/monad/list)

(define-interface <list> (<monad>)
  ()
  (:singleton)
  (:method> result (value) (list value))
  (:method> bind (mvs mf)
	    (loop :for mv in mvs
		  :append (funcall mf mv))))

