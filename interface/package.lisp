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

   ;;; Macros
   #:define-interface
   #:make-interface
   #:with-interface
   #:define-interface-specialized-functions
   #:define-interface-methods

   ;;; General purpose gfs
   #:update
   #:base-interface #:key-interface #:value-interface

   ;; Base
   #:<type> #:check-invariant #:convert
   #:<any>
   #:<classy>

   ;;; Algebra
   #:<magma> #:op
   #:<semigroup> #:op/list
   #:<monoid> #:id
   #:<group> #:inverse

   ;; Base Collections
   #:<copyable> #:copy
   #:<emptyable> #:empty #:empty-p
   #:<foldable> #:monoid-fold #:monoid-fold* #:fold-left #:fold-right #:fold-left* #:fold-right* #:for-each #:for-each*
   #:<makeable> #:make
   #:<sizable> #:size #:size<=n-p
   #|#:<finite-collection>|# #:singleton-p #:singleton #:singleton* #:first-entry #:entry-values
   #:<encoded-key-collection> #:<parametric-encoded-key-collection> #:encode-key #:decode-key

   ;;; eq
   #:<eq> #:== #:eq-function
   #:<hashable> #:hash
   #:<eql> #:<equal>
   #:<eq-from-==> #:<eq-from-eq-function>

   ;;; order
   #:<order> #:<order-from-lessp> #:<lessp>
   #:<order-from-compare> #:<compare>
   #:order< #:order<= #:order> #:order>= #:== #:compare
   #:<order-parameter> #:order-interface
   #:<number> #:<integer> #:<char> #:<string> #:<case-insensitive-string>

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
   #|#:<map>|# #:lookup #:first-key-value #:map-alist #:alist-map
   #|#:<set>|# #:member-p #:set-list #:list-set
   #|#:<multiset>|# #:member-count #:multiset-list #:list-multiset
   #|#:<sequence>|# #:sequence-list #:list-sequence
   #:node-class
   #:node #:node-key #:node-value #:left #:right #:node-balance #:node-height
   #:locate #:node-key-value #:leftmost-node #:rightmost-node #:leftmost #:rightmost #:leftmost-entry #:rightmost-entry

   ;; number iterators
   #:make-number-iterator
   #:<number-iterator> #:<decreasing-number-iterator> #:<increasing-number-iterator>
   #:iterator-start #:iterator-end #:iterator-increment
   ;; TODO: move this somewhere else
   #:boolean-integer

   ;; simple mixins
   #:<copy-is-identity>
   #:<empty-is-nil>
   #:<empty-is-empty-object> #:empty-object #:make-empty-object #:empty-object-p
   #:<foldable-*-from>
   #:<foldable-fold-monoid-from-fold-left>
   #:<foldable-fold-right-from-fold-left>
   #:<foldable-for-each-from-fold-left>
   #:<foldable-size-from-fold-left>
   #:<map-foldable-from-*>
   #:<map-for-each*-from-fold-left*>
   #:<map-fold-right*-from-fold-left*>
   #:<map-monoid-fold*-from-fold-left*>
   #:<sizable-size<=n-p-from-size>

   ;; linearize
   #:define-linearized-interface #:define-linearized-method #:stateful-interface
   ;; mutating
   #:define-mutating-interface #:define-mutating-method #:pure-interface
   ;; classify
   #:define-classified-interface-class #:define-classified-method #:object-box))
