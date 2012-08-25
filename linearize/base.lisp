;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Pure to Stateful and back

#+xcvb (module (:depends-on ("linearize/linearize" "pure/map-interface")))

(in-package :pure)

(define-interface <linear> (<interface>) ())

(define-interface <linearized> (<linear>)
  ((stateful-interface
    :reader stateful-interface
    :initarg :stateful-interface)
   #|(box-interface
    :reader box-interface
    :initarg :box-interface :initform <one-use-value-box>)|#)
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))
