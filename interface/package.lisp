;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module ())

(in-package :cl)

(defpackage :interface
  (:use :closer-common-lisp :fare-memoization :xcvb-utils :closer-mop)
  (:export

   ;;; Classes
   #:interface-class
   #:<interface>
   #:<type>
   #:<classy>

   ;;; Macros
   #:define-interface
   #:make-interface
   #:with-interface
   #:define-interface-specialized-functions
   #:define-interface-methods

   ;;; General purpose gfs
   #:check-invariant
   #:make
   #:update
   #:base-interface
   #:instantiate
   #:convert
   #:size #:size<=n-p
   #:for-each

   ;;; Empty?
   #:<emptyable> #:empty #:empty-p
   #:<empty-is-nil>
   #:<empty-is-empty-object>

   ;;; eq
   #:<eq> #:== #:eq-function
   #:<hashable> #:hash
   #:<eql> #:<equal>
   #:<eq-from-==> #:<eq-from-eq-function>

   ;;; order
   #:<order> #:<number> #:<string> #:<char> #:<case-insensitive-string>
   #:<order-from-lessp> #:<lessp>
   #:<order-from-compare> #:<compare>
   #:order< #:order<= #:order> #:order>= #:== #:compare
   #:<order-parameter> #:order-interface

   ;;; Boxes!
   #:box #:box-ref #:box-set!
   #:box-value #:set-box-value #:box!
   #:<box> #:make-box #:unbox
   #:<classy-box>
   #:<value-box> #:value-box #:simple-value-box ;;#:peek
   #:<thunk-box> #:thunk-box #:simple-thunk-box
   #:<promise-box> #:promise-box #:delay #:force
   #:<one-use-box> #:one-use-box
   #:<one-use-value-box> #:one-use-value-box
   #:<one-use-thunk-box> #:one-use-thunk-box
   #:make-one-use-function #:one-use-lambda
   #:<emptyable-box> #:empty #:empty-p
   #:<mutable-box> #:mutable-box #:immutable-box #:set-box!
   #:<box!> #:box!

   ;;; Maps, trees, etc.
   ;; Do NOT export these interfaces, because pure and stateful branch them.
   ;; #:<map> #:<alist> #:<tree> #:<binary-tree> #:<avl-tree> #:<number-map> #:<nm> #:<fmim>
   ;; DO export the accessors, because pure and stateful use them.
   #:lookup #:first-key-value #:fold-left #:fold-right #:map-alist #:alist-map
   #:node #:node-key #:node-value #:left #:right #:node-height #:node-balance
   #:locate #:node-key-value #:leftmost-node #:rightmost-node #:leftmost #:rightmost
   #:key-interface #:value-interface

   ;; number iterators
   #:make-number-iterator
   #:<number-iterator> #:<decreasing-number-iterator> #:<increasing-number-iterator>
   #:iterator-start #:iterator-end #:iterator-increment
   ;; TODO: move this somewhere else
   #:boolean-integer

   #:node-class
   #:empty-object #:make-empty-object #:empty-object-p

   ;; simple mixins
   #:<sizable-size<=n-p-from-size>
   #:<map-for-each-from-fold-left>
   #:<map-fold-right-from-fold-left>
   #:<map-size-from-fold-left>

   ;; linearize
   #:define-linearized-interface #:define-linearized-method #:stateful-interface
   ;; mutating
   #:define-mutating-interface #:define-mutating-method #:pure-interface
   ;; classify
   #:define-classified-interface-class #:define-classified-method #:object-box))
