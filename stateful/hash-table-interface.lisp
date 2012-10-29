;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("interface/eq" "stateful/map-interface")))

(in-package :stateful)

(define-interface <hash-table>
    (<map-copy-from-join-empty>
     <map-decons-from-first-key-value-drop>
     <map-divide-from-for-each>
     <map-divide/list-from-divide>
     <map-first-key-value-from-for-each>
     <map-fold-left-from-for-each>
     <map-fold-right-from-fold-left>
     <map-join-from-for-each-lookup-insert>
     <map-join/list-from-join>
     <map-map/2-from-for-each-lookup-insert-drop>
     <map-update-key-from-lookup-insert-drop>
     <sizable-size<=n-p-from-size>
     <map>)
  ((key-interface :reader key-interface :initarg :key-interface))
  ;; we would default to <eql>, but there is no standard hash function associated to it, for
  ;; there is no standard way to re-hash the table after a GC that would disturb pointer values.
  ;; So instead we default to <equal>.
  (:parametric (&key (key <equal>)) (make-interface :key-interface key))
  (:singleton)
  (:documentation "stateful hash table"))
