;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; trees -- what's common between pure and stateful trees

#+xcvb (module (:depends-on ("interface/map-interface" "interface/order")))

(in-package :interface)

;;; Trees in general

(define-interface <tree> (<map>) ()
  (:documentation "abstract interface for trees"))

#| ;;; TODO: do we want a node interface associated to the tree interface?
(define-interface <node> (<type>) ()
  (:documentation "abstract interface for nodes of trees"))
(defgeneric node-interface (<tree>)
  (:documentation "returns the interface for nodes of given tree interface"))
(defgeneric key-interface (<interface>)
  (:documentation "returns the interface for keys of given tree interface"))
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
    (<tree> <map> order:<order>) ;; TODO: delegate that to a key interface?
  ()
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

;;; pure AVL-tree

(define-interface <avl-tree> (<binary-tree>) ())

(defclass avl-tree-node (binary-tree-node)
  ((height
    :initarg :height
    :initform 0
    :type integer
    :reader node-height)))

(defgeneric node-balance (node))

;;; Common special case: when keys are (real) numbers
(define-interface <number-map> (<avl-tree> order:<number>) ())
