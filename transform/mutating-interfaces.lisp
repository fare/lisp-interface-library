;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Pure to Stateful by Boxification: actual interfaces

#+xcvb
(module (:depends-on ("transform/mutating" "pure/map-interface" "stateful/map-interface")))

(in-package :stateful)

(define-interface <mutating> (<interface>)
  ((pure-interface
    :reader pure-interface
    :initarg :pure-interface))
  (:parametric (interface)
    (make-interface :pure-interface interface)))

(define-mutating-interface <mutating-map> (<map>) (pure:<map>)
  ()
  (:method empty! (x)
     (interface::set-box-value (empty (pure-interface <mutating-map>)) x))
  (:parametric (interface)
    (make-interface :pure-interface interface)))
