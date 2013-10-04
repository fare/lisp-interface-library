;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; All stateful interfaces and data structures

(uiop:define-package :lil/stateful/all
  (:nicknames :stateful)
  (:import-from :lil/interface/all)
  (:use :closer-common-lisp)
  (:mix :fare-utils :uiop :alexandria)
  (:use-reexport
   :lil/interface/base
   :lil/interface/eq
   :lil/interface/order
   :lil/interface/group
   :lil/stateful/empty
   :lil/stateful/collection
   :lil/stateful/iterator
   :lil/stateful/map
   :lil/stateful/tree
   :lil/stateful/hash-table
   :lil/stateful/encoded-key-map
   :lil/stateful/queue
   :lil/stateful/iterator-implementation
   :lil/stateful/map-implementation
   :lil/stateful/tree-implementation
   :lil/stateful/hash-table-implementation
   :lil/stateful/encoded-key-map-implementation
   :lil/stateful/queue-implementation))
