;;;;; Pure trees - interface

(uiop:define-package :lil/pure/tree
  (:use :closer-common-lisp
        :core
        :lil/interface/base
        :lil/interface/order)
  (:use-reexport
   :lil/interface/tree
   :lil/pure/map)
  (:shadow #:<tree> #:<binary-tree> #:<heighted-binary-tree> #:<avl-tree> #:<number-map>
           #:association-pair #:binary-tree-node #:avl-tree-node)
  (:export
   #:<tree> #:<binary-tree> #:<avl-tree> #:<parametric-avl-tree> #:<heighted-binary-tree>
   #:<number-map> #:<nm> #:<string-map>
   #:association-pair #:binary-tree-node #:avl-tree-node))
(in-package :lil/pure/tree)

(define-interface <tree> (lil/interface/tree:<tree> <map>) ()
  (:abstract)
  (:documentation "abstract interface for trees"))

;;; Vanilla Binary Tree

(define-interface <binary-tree>
    (<tree> lil/interface/tree:<binary-tree>
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

(define-interface <parametric-avl-tree> (<avl-tree>)
  ((key-interface :type <order> :reader key-interface :initarg :key-interface)
   (value-interface :type <type> :reader value-interface :initarg :value-interface))
  (:parametric (key-interface &optional (value-interface <any>))
     (make-interface :key-interface key-interface :value-interface value-interface)))

;;; Common special cases: when keys are (real) numbers, strings
(define-interface <number-map> (<avl-tree> lil/interface/tree:<number-map>)
  ()
  (:singleton))

(defparameter <nm> <number-map>)

(defparameter <string-map> (<parametric-avl-tree> <string>))
