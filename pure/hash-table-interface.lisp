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
    (<copy-is-identity>
     <map-foldable-from-*>
     <map-has-key-p-from-lookup>
     <map-join-from-fold-left*-insert>
     <map-join/list-from-join>
     <map-update-key-from-lookup-insert-drop>
     <map-map/2-from-fold-left*-lookup-insert-drop>
     <map-singleton-from-insert>
     <map>)
  ((key-interface :type <hashable>
    :reader key-interface :initarg :key-interface)
   (value-interface :type <type>
    :reader value-interface :initarg :value-interface)
   (hashmap-interface :type <map>
    :reader hashmap-interface :initarg :hashmap-interface)
   (bucketmap-interface :type <map>
    :reader bucketmap-interface :initarg :bucketmap-interface))
  (:parametric (&key (key-interface <equal>) (value-interface <any>)
		     (hashmap-interface <number-map>)
		     (bucketmap-interface (<alist> key-interface value-interface)))
     (make-interface :key-interface key-interface
		     :value-interface value-interface
		     :hashmap-interface hashmap-interface
		     :bucketmap-interface bucketmap-interface))
  (:singleton)
  (:documentation "pure hash table"))
