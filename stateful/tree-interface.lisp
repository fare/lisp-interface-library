;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees - interface

#+xcvb (module (:depends-on ("stateful/map-interface")))

(in-package :stateful)

;;; Trees in general

(define-interface <tree> (interface::<tree> <map>) ()
  (:documentation "abstract interface for stateful trees"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree> interface::<binary-tree>
     map-simple-empty ;; handles all the empty-object cases so we don't have to.
     map-simple-decons map-simple-update-key
     map-simple-join map-simple-map/2 map-simple-join/list
     map-size-from-fold-left)
  ()
  (:documentation "Keys in binary trees increase from left to right"))

(defclass association-pair (interface::association-pair)
  ((value :accessor node-value))) ;; writable value, not writable key.

(defclass binary-tree-node (interface::binary-branch association-pair)
  ((left :initform (make-empty-object))
   (right :initform (make-empty-object))))

;;; pure AVL-tree

(define-interface <avl-tree> (<binary-tree> interface::<avl-tree>) ())

(defclass avl-tree-node (interface::avl-tree-node binary-tree-node) ())

;;; Common special case: when keys are (real) numbers
(define-interface <number-map> (<avl-tree> interface::<number-map>)
  ()
  (:singleton))

(defparameter <nm> <number-map>)
