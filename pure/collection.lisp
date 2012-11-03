;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Base collections

#+xcvb (module (:depends-on ("pure/iterator-interface")))

(in-package :pure)

(define-interface <empty!able> (<emptyable>) ()
  (:generic> empty! (ignored) (:in 1)
    (:values empty) (:out 0)
    (:method> (ignored)
       (declare (ignorable ignored))
       (empty))
    (:documentation
     "This function is pretty useless to call, but allows for
      automatic generation of mutating interface wrappers.")))
