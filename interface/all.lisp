;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic Interfaces

#+xcvb (module (:depends-on ("interface/definition")))

(uiop:define-package :lil/interface/all
  (:use
   :lil/interface/utility
   :lil/interface/definition
   :lil/interface/base
   :lil/interface/box
   :lil/interface/eq
   :lil/interface/order
   :lil/interface/iterator
   :lil/interface/map
   :lil/interface/set
   :lil/interface/tree
   :lil/interface/tree-implementation
   :lil/interface/tree
   :lil/interface/zero-plus
   :lil/interface/monad
   :lil/interface/run
   :lil/interface/monad/continuation
   :lil/interface/monad/identity
   :lil/interface/monad/list
   :lil/interface/monad/maybe
   :lil/interface/monad/state
   :lil/interface/monad/transformer/list
   :lil/interface/monad/transformer/maybe)
  (:reexport
   :lil/interface/utility
   :lil/interface/definition
   :lil/interface/base
   :lil/interface/box
   :lil/interface/eq
   :lil/interface/order
   :lil/interface/iterator
   :lil/interface/map
   :lil/interface/set
   :lil/interface/tree
   :lil/interface/tree-implementation
   :lil/interface/zero-plus
   :lil/interface/monad
   :lil/interface/run
   :lil/interface/monad/continuation
   :lil/interface/monad/identity
   :lil/interface/monad/list
   :lil/interface/monad/maybe
   :lil/interface/monad/state
   :lil/interface/monad/transformer/list
   :lil/interface/monad/transformer/maybe))
