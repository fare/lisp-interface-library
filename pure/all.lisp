;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; All pure interfaces and data structures

(uiop:define-package :lil/pure/all
  (:import-from :lil/interface/all)
  (:use-reexport
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
