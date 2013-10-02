;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; trees -- what's common between pure and stateful trees

#+xcvb (module (:depends-on ("interface/order" "interface/map")))

(uiop:define-package :lil/interface/tree
  (:use :closer-common-lisp
   :lil/interface/definition :lil/interface/base :lil/interface/map :lil/interface/order)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:<node> ;; to be shadowed by pure, stateful packages. Move node to its own file?
   #:node-class #:node-key-value #:leftmost-node #:leftmost #:rightmost-node #:rightmost
   #:locate #:node
   ;; to be shadowed by pure, stateful packages:
   #:<tree> #:<binary-tree> #:<avl-tree> #:<number-map>
   #:binary-branch #:binary-tree-node #:association-pair #:heighted-binary-tree-node #:avl-tree-node
   ;; not shadowed:
   #:node-balance
   ))

(in-package :lil/interface/tree)

;;; Trees in general

(define-interface <tree> (<map>) ()
  (:abstract)
  (:documentation "abstract interface for trees"))

#| ;;; TODO: do we want a node interface associated to the tree interface?
(define-interface <node> (<type>) ()
  (:abstract)
  (:documentation "abstract interface for nodes of trees"))
(defgeneric node-interface (<tree>)
  (:documentation "returns the interface for nodes of given tree interface"))
|#

;; We should probably be using a node interface instead, but this will do for now
(defgeneric node-class (<interface>)
  (:documentation "return the node class associated with an interface."))

(defgeneric node-key-value (<tree> tree)
  (:documentation "key and value associated with a node in a TREE"))

(defgeneric leftmost-node (<tree> tree)
  (:documentation "the leftmost node in TREE"))

(defgeneric leftmost (<tree> tree)
  (:documentation "key, value and foundp from the leftmost node in TREE"))

(defgeneric rightmost-node (<tree> tree)
  (:documentation "the rightmost node in TREE"))

(defgeneric rightmost (<tree> tree)
  (:documentation "key, value and foundp from the rightmost node in TREE"))

(defgeneric locate (<tree> tree key path)
  (:documentation "lookup a tree for a key, return a path to the proper node."))

(defgeneric node (<tree> &key #+sbcl &allow-other-keys)
  (:documentation "make a node for a tree interface"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree>
     <foldable-size-from-fold-left>
     <map-foldable-from-*>
     <map>) ()
  (:abstract)
  (:documentation "Keys in binary trees increase from left to right"))

(defclass binary-branch ()
  ((left
    :initarg :left
    :reader left)
   (right
    :initarg :right
    :reader right)))

(defclass association-pair ()
  ((key
    :initarg :key
    :reader node-key)
   (value
    :initarg :value
    :reader node-value)))

(defclass binary-tree-node (binary-branch association-pair)
  ;;; Or should we have a box instead of an association-pair ???
  ;;; Or let the user just inherit from binary-branch,
  ;;; and use a node-interface with make and update?
  ())

;;; Trees that maintain a balance
(defgeneric node-balance (node))

;;; Trees that maintain a record of their height
(define-interface <heighted-binary-tree> (<binary-tree>) ()
  (:abstract))

(defclass heighted-binary-tree-node (binary-tree-node)
  ((height
    :initarg :height
    :initform 0
    :type integer
    :reader node-height)))

;;; base AVL-tree

(define-interface <avl-tree> (<heighted-binary-tree>) ()
  (:abstract))

(defclass avl-tree-node (heighted-binary-tree-node) ())

;;; Common special case: when keys are (real) numbers
(define-interface <number-map> (<avl-tree>)
  ((key-interface :allocation :class :initform <number> :reader key-interface))
  (:abstract)) ;; must be pure or stateful!
