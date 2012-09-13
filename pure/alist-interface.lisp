;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map interface: alists.

#+xcvb (module (:depends-on ("interface/eq" "pure/map-interface")))

(in-package :pure)

(define-interface <alist>
    (map-simple-empty map-simple-decons map-simple-update-key
     map-divide/list-from-divide
     map-simple-map/2 map-simple-join map-simple-join/list <map>)
  ((key-interface
    :initarg :key-interface
    :initform <eql>
    :reader key-interface))
  (:parametric (&optional (eq <eql>)) (make-interface :key-interface eq))
  (:singleton))
