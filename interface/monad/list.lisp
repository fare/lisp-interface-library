;;; Interface Passing Style : Monad : List

(defpackage :interface/monad/list
  (:nicknames :drewc.org/ips/monad/list)
  (:use :cl :interface/monad)
  (:export #:<list>))

(in-package :interface/monad/list)

(interface:define-interface <list> (<monad>)
  ()
  (:singleton)
  (:method> result (value) (list value))
  (:method> bind (mvs mf)
	    (loop :for mv in mvs
		  :append (funcall mf mv))))


