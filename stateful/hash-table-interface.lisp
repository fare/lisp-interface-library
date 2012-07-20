;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("interface/eq" "stateful/map-interface")))

(in-package :stateful)

(define-interface <hash-table>
    (<map>
     map-simple-join map-simple-decons map-simple-update-key map-simple-map/2
     map-fold-right-from-fold-left map-cheap-size map-divide/list-from-divide
     map-divide-from-for-each map-first-key-value-from-for-each
     map-fold-left-from-for-each)
  ((key-interface :reader key-interface :initarg :key))
  (:parametric (&key (key eq:<equal>)) (make-interface :key key))
  (:singleton)
  (:documentation "stateful hash table"))
