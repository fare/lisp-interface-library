;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map interface: alists.

#+xcvb (module (:depends-on ("interface/eq" "pure/map-interface")))

(in-package :pure)

(define-interface <alist>
    (<map>
     map-simple-empty map-simple-decons map-simple-update-key
     map-divide/list-from-divide
     map-simple-map/2 map-simple-join map-simple-join/list)
  ((eq-interface
    :initarg :eq
    :initform eq:<eq>
    :reader eq-interface))
  (:parametric (&optional (eq eq:<eq>)) (make-interface :eq eq))
  (:singleton))
