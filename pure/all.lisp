;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; All pure interfaces and data structures

(uiop:define-package :lil/pure/all
  (:import-from :lil/interface/all)
  (:use-reexport
   :lil/pure/empty
   :lil/pure/collection
   :lil/pure/iterator
   :lil/pure/iterator-implementation
   :lil/pure/map
   :lil/pure/map-implementation
   ))
