;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; All general interfaces + common core of pure and stateful data structures

(uiop:define-package :lil/interface/all
  (:nicknames :interface)
  (:use :closer-common-lisp)
  (:mix :fare-utils :uiop :alexandria)
  (:use-reexport
   :lil/interface/utility
   :lil/interface/definition
   :lil/interface/base
   :lil/interface/group
   :lil/interface/empty
   :lil/interface/size
   :lil/interface/fold
   :lil/interface/collection
   :lil/interface/box
   :lil/interface/eq
   :lil/interface/order
   :lil/interface/iterator
   :lil/interface/sequence
   :lil/interface/map
   :lil/interface/set
   :lil/interface/tree
   :lil/interface/tree-implementation
   :lil/interface/tree
   :lil/interface/monad
   :lil/interface/run
   :lil/interface/monad/continuation
   :lil/interface/monad/identity
   :lil/interface/monad/list
   :lil/interface/monad/maybe
   :lil/interface/monad/state
   :lil/interface/monad/transformer/list
   :lil/interface/monad/transformer/maybe))
