;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful trees - interface

(uiop:define-package :lil/stateful/tree
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/interface/tree
   :lil/stateful/map)
  (:shadow #:<tree> #:<binary-tree> #:<avl-tree> #:<heighted-binary-tree> #:<number-map>
           #:association-pair #:binary-tree-node #:binary-branch
           #:heighted-binary-tree-node #:avl-tree-node)
  (:export
   #:<tree> #:<binary-tree> #:<avl-tree> #:<heighted-binary-tree> #:<number-map> #:<nm>
   #:association-pair #:binary-tree-node #:binary-branch
   #:heighted-binary-tree-node #:avl-tree-node
   #:<post-self-balanced-binary-tree>
   #:balance-node #:rotate-node-left #:rotate-node-right #:update-height))
(in-package :lil/stateful/tree)

;;; Trees in general

(define-interface <tree> (lil/interface/tree:<tree> <map>) ()
  (:abstract)
  (:documentation "abstract interface for stateful trees"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree>
     lil/interface/tree:<binary-tree>
     <foldable-size-from-fold-left>
     <map-copy-from-join-empty>
     <map-empty-is-empty-object> ;; handles all the empty-object cases so we don't have to.
     <map-decons-from-first-key-value-drop>
     <map-update-key-from-lookup-insert-drop>
     <map-join-from-for-each*-lookup-insert>
     <map-join/list-from-join>
     <map-map/2-from-for-each*-lookup-insert-drop>
     <map>)
  ()
  (:abstract)
  (:documentation "Keys in binary trees increase from left to right"))

(defclass binary-branch (lil/interface/tree:binary-branch)
  ((left :accessor left :initform (make-empty-object))
   (right :accessor right :initform (make-empty-object))))

(defclass association-pair (lil/interface/tree:association-pair)
  ((lil/interface/tree:key :accessor node-key) ;; only write the key when copying a key-value pair.
   (lil/interface/tree:value :accessor node-value))) ;; writable value, not writable key.

(defclass binary-tree-node (binary-branch association-pair) ())

;;; Balancing trees
(defgeneric balance-node (<tree> node)
  (:documentation "balance a node in a tree"))

;; We really ought to either do *everything* in detached interface-passing style,
;; *or* keep it in subject-oriented code but split it in a different package,
;; and provide some hooks between the two.
(defgeneric rotate-node-right (node))
(defgeneric rotate-node-left (node))

(define-interface <post-self-balanced-binary-tree> (<binary-tree>) ()
  (:abstract))

;;; Trees that maintain a record of their height
(define-interface <heighted-binary-tree> (lil/interface/tree:<heighted-binary-tree> <binary-tree> ) ()
  (:abstract))

(defclass heighted-binary-tree-node (lil/interface/tree:heighted-binary-tree-node binary-tree-node)
    ((lil/interface/tree:height :accessor node-height))) ;; make it writable.

(defgeneric update-height (node))

;;; stateful AVL-tree

(define-interface <avl-tree>
    (lil/interface/tree:<avl-tree>
     <heighted-binary-tree>
     <post-self-balanced-binary-tree>) ()
  (:abstract))

(defclass avl-tree-node (lil/interface/tree:avl-tree-node heighted-binary-tree-node) ())

;;; Common special case: when keys are (real) numbers
(define-interface <number-map> (lil/interface/tree:<number-map> <avl-tree>)
  ()
  (:singleton))

(defparameter <nm> <number-map>)
