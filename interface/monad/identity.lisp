;;; Interface Passing Style : Monad : Identity

(defpackage :interface/monad/identity
  (:nicknames :drewc.org/ips/monad/identity)
  (:use :cl :interface/monad)
  (:export #:<identity>))

(in-package :interface/monad/identity)

(interface:define-interface <identity> (<monad>)
  ()
  (:singleton)
  (:documentation "The RESULT function for <IDENTITY> is like CL:IDENTITY, and
simply returns the value passed to it.

The BIND function takes a monadic value, which in the case of
<IDENTITY> is anything, and a monadic function, which is a one
argument function. It FUNCALLs the function with the value, and
returns the result.")
  (:method> result (value)
   "Returns the VALUE. like CL:IDENTITY"
   VALUE)
  (:method> bind (mv mf)
    "FUNCALLs the MF with the MV"
    (funcall mf mv)))

