;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb
(module
 (:depends-on
  ("interface/interface"
   "interface/eq"
   "pure/package"
   "pure/map-interface"
   "pure/alist-interface"
   "pure/tree-interface")))

(in-package :pure)

(define-interface <hash-table>
    (<map>
     map-simple-join map-simple-update-key map-simple-map/2)
  ((key-interface :reader key-interface :initarg :key)
   (hashmap-interface :reader hashmap-interface :initarg :hashmap)
   (bucketmap-interface :reader bucketmap-interface :initarg :bucketmap))
  (:parametric (&key (key eq:<equal>)
                     (hashmap <number-map>)
                     (bucketmap (<alist> key)))
               (make-interface :key key :hashmap hashmap :bucketmap bucketmap))
  (:singleton)
  (:documentation "pure hash table"))
