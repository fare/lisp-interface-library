;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees - interface

#+xcvb (module (:depends-on ("pure/alist-interface")))

(in-package :pure)

;;; Trees in general

(define-interface <tree> (<type>) ()
  (:documentation "abstract interface for trees"))

#|
(defclass <node> (<type>) ()
  (:documentation "abstract interface for nodes of trees"))
(defgeneric node-interface (<tree>)
  (:documentation "returns the interface for nodes of given tree interface"))
(defgeneric key-interface (<interface>)
  (:documentation "returns the interface for keys of given tree interface"))
|#

(defgeneric leftmost (<tree> tree)
  (:documentation "key, value and foundp from the leftmost node in TREE"))

(defgeneric rightmost (<tree> tree)
  (:documentation "key, value and foundp from rightmost node in TREE"))

(defgeneric locate (<tree> tree key path)
  (:documentation "lookup a tree for a key, return a path to the proper node."))

(defgeneric node (<tree> &key)
  (:documentation "make a node for a tree interface"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree> <map>
     order:<order> ;; TODO: delegate that to a key interface?
     map-simple-empty ;; handles all the null cases so we don't have to.
     map-simple-decons map-simple-update-key
     map-simple-join map-simple-map/2 map-simple-join/list
     map-simple-size)
  ()
  (:documentation "Keys in binary trees increase from left to right"))

(defclass binary-branch ()
  ((left
    :initarg :left
    :initform nil
    :reader left)
   (right
    :initarg :right
    :initform nil
    :reader right)))

(defclass association-pair ()
  ((key
    :initarg :key
    :initform nil
    :reader node-key)
   (value
    :initarg :value
    :initform nil
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
(define-interface <number-map> (<avl-tree> order:<number>)
  ()
  (:singleton))

(defparameter <nm> <number-map>)
