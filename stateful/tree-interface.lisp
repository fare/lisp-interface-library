;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees - interface

#+xcvb (module (:depends-on ("stateful/map-interface" "interface/tree-interface")))

(in-package :stateful)

;;; Trees in general

(define-interface <tree> (interface::<tree> <map>) ()
  (:abstract)
  (:documentation "abstract interface for stateful trees"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree> interface::<binary-tree>
     map-simple-empty ;; handles all the empty-object cases so we don't have to.
     map-simple-decons map-simple-update-key
     map-simple-join map-simple-map/2 map-simple-join/list
     map-size-from-fold-left)
  ()
  (:abstract)
  (:documentation "Keys in binary trees increase from left to right"))

(defclass binary-branch (interface::binary-branch)
  ((left :accessor left :initform (make-empty-object))
   (right :accessor right :initform (make-empty-object))))

(defclass association-pair (interface::association-pair)
  ((interface::key :accessor node-key) ;; only write the key when copying a key-value pair.
   (interface::value :accessor node-value))) ;; writable value, not writable key.

(defclass binary-tree-node (binary-branch association-pair) ())

;;; Balancing trees
(defgeneric balance-node (<tree> node)
  (:documentation "balance a node in a tree"))

;; We really ought to either do *everything* in detached interface-passing style,
;; *or* keep it in subject-oriented code but split it in a different package,
;; and provide some hooks between the two.
(defgeneric rotate-node-right (node))
(defgeneric rotate-node-left (node))

;;; pure AVL-tree

(define-interface <post-self-balanced-binary-tree> (<binary-tree>) ()
  (:abstract))

(define-interface <avl-tree> (<post-self-balanced-binary-tree> interface::<avl-tree>) ()
  (:abstract))

(defclass avl-tree-node (interface::avl-tree-node binary-tree-node)
  ((interface::height :accessor node-height))) ;; make it writable.

(defgeneric update-height (node))


;;; Common special case: when keys are (real) numbers
(define-interface <number-map> (<avl-tree> interface::<number-map>)
  ()
  (:singleton))

(defparameter <nm> <number-map>)
