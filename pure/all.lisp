;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; All pure interfaces and data structures

(uiop:define-package :lil/pure/all
  (:nicknames :pure)
  (:import-from :lil/interface/all)
  (:use :closer-common-lisp)
  (:mix :fare-utils :uiop :alexandria)
  (:use-reexport
   :lil/interface/base
   :lil/interface/eq
   :lil/interface/order
   :lil/interface/group
   :lil/pure/empty
   :lil/pure/collection
   :lil/pure/iterator
   :lil/pure/map
   :lil/pure/set
   :lil/pure/alist
   :lil/pure/tree
   :lil/pure/hash-table
   :lil/pure/fmim
   :lil/pure/encoded-key-map
   :lil/pure/queue
   :lil/pure/iterator-implementation
   :lil/pure/map-implementation
   :lil/pure/set-implementation
   :lil/pure/alist-implementation
   :lil/pure/tree-implementation
   :lil/pure/hash-table-implementation
   :lil/pure/fmim-implementation
   :lil/pure/encoded-key-map-implementation
   :lil/pure/queue-implementation
   ))
