;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb
(module
 (:depends-on
  ("interface/interface"
   "interface/eq"
   "pure/map-interface"
   "pure/alist-interface"
   "pure/tree-interface")))

(in-package :pure)

(define-interface <hash-table>
    (<map-join-from-fold-left-insert>
     <map-join/list-from-join>
     <map-update-key-from-lookup-insert-drop>
     <map-map/2-from-fold-left-lookup-insert-drop>
     <map>)
  ((key-interface
    :type <hashable>
    :reader key-interface :initarg :key)
   (hashmap-interface
    :type <map>
    :reader hashmap-interface :initarg :hashmap)
   (bucketmap-interface
    :type <map>
    :reader bucketmap-interface :initarg :bucketmap))
  (:parametric (&key (key <equal>)
                     (hashmap <number-map>)
                     (bucketmap (<alist> key)))
               (make-interface :key key :hashmap hashmap :bucketmap bucketmap))
  (:singleton)
  (:documentation "pure hash table"))
