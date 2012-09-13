;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("interface/eq" "stateful/map-interface")))

(in-package :stateful)

(define-interface <hash-table>
    (map-simple-join map-simple-decons map-simple-update-key map-simple-map/2
     map-fold-right-from-fold-left map-cheap-size map-divide/list-from-divide
     map-divide-from-for-each map-first-key-value-from-for-each
     map-fold-left-from-for-each <map>)
  ((key-interface :reader key-interface :initarg :key-interface))
  ;; we would default to <eql>, but there is no standard hash function associated to it, for
  ;; there is no standard way to re-hash the table after a GC that would disturb pointer values.
  ;; So instead we default to <equal>.
  (:parametric (&key (key <equal>)) (make-interface :key-interface key))
  (:singleton)
  (:documentation "stateful hash table"))
