;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees - interface

(uiop:define-package :lil/pure/tree
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/interface/tree
   :lil/pure/map)
  (:shadow #:<tree> #:<avl-tree> #:<number-map>
           #:association-pair #:binary-tree-node #:avl-tree-node)
  (:export
   #:<tree> #:<avl-tree> #:<number-map> #:<nm>
   #:association-pair #:binary-tree-node #:avl-tree-node))
(in-package :lil/pure/tree)

(define-interface <tree> (lil/interface/tree:<tree> <map>) ()
  (:abstract)
  (:documentation "abstract interface for trees"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree>
     <map-decons-from-first-key-value-drop>
     <map-empty-is-nil> ;; handles all the null cases so we don't have to.
     <map-has-key-p-from-lookup>
     <map-join-from-fold-left*-insert>
     <map-join/list-from-join>
     <map-map/2-from-fold-left*-lookup-insert-drop>
     <map-monoid-fold*-from-divide/list>
     <map-update-key-from-lookup-insert-drop>)
  ()
  (:abstract)
  (:documentation "Keys in binary trees increase from left to right"))

(defclass association-pair (lil/interface/tree:association-pair)
  ())

(defclass binary-tree-node (lil/interface/tree:binary-tree-node association-pair)
  ;;; Or should we have a box instead of an association-pair ???
  ;;; Or let the user just inherit from binary-branch,
  ;;; and use a node-interface with make and update?
  ((left :initform nil)
   (right :initform nil)))

;;; pure AVL-tree

(define-interface <avl-tree> (lil/interface/tree:<avl-tree> <binary-tree>) ()
  (:abstract))

(defclass avl-tree-node (lil/interface/tree:avl-tree-node binary-tree-node)
  ())

;;; Common special case: when keys are (real) numbers
(define-interface <number-map> (<avl-tree> lil/interface/tree:<number-map>)
  ()
  (:singleton))

(defparameter <nm> <number-map>)
