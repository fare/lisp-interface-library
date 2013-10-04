;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful trees (implementation)

(uiop:define-package :lil/stateful/tree-implementation
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base
        :lil/interface/order)
  (:use-reexport
   :lil/stateful/tree))
(in-package :lil/stateful/tree-implementation)

(defmethod node-class ((i <binary-tree>))
  'binary-tree-node)

(defmethod node ((i <binary-tree>) &key left right key value)
  (make-instance (node-class i)
                 :key key :value value :left left :right right))

(defmethod insert ((i <binary-tree>) node key value)
  (cond
    ((empty-p i node)
     (change-class node (node-class i) :key key :value value)
     (values))
    (t
     (ecase (compare (key-interface i) key (node-key node))
       (0 (setf (node-value node) value) (values))
       (-1 (insert i (left node) key value))
       (1 (insert i (right node) key value))))))

(defgeneric copy-node (destination-node origin-node))

(defmethod copy-node ((destination-node binary-tree-node) (origin-node empty-object))
  (change-class destination-node 'empty-object)
  (values))

(defmethod copy-node ((destination-node binary-tree-node) (origin-node binary-tree-node))
  (setf (left       destination-node) (left       origin-node)
        (right      destination-node) (right      origin-node)
        (node-key   destination-node) (node-key   origin-node)
        (node-value destination-node) (node-value origin-node))
  (values))

(defmethod drop ((i <binary-tree>) node key)
  (if (empty-p i node)
      (values nil nil)
      (let ((k (node-key node))
            (v (node-value node)))
        (ecase (compare (key-interface i) key k)
          (0 (cond
               ((empty-p i (left node))
                (copy-node node (right node)))
               ((empty-p i (right node))
                (copy-node node (left node)))
               (t
                ;; TODO: have a way to pick (based on balance information?)
                ;; which of "leftmost of the right side"
                ;; and "rightmost of the left side" to pick
                (multiple-value-bind (kk vv)
                    (leftmost i (right node))
                  (drop i (right node) kk)
                  (setf (node-key node) kk
                        (node-value node) vv))))
             (values v t))
          (-1
           (drop i (left node) key))
          (1
           (drop i (right node) key))))))

(defmethod divide ((i <binary-tree>) node)
  (if (empty-p i node)
      (values (empty i) node)
      (let ((left (left node))
	    (right (right node)))
	(if (empty-p i left)
	    (progn
	      (setf (right node) (empty i))
	      (values right node))
	    (progn
	      ;; We could "just" cut the non-empty left side,
	      ;; and return it and the now very unbalanced tree.
	      ;; But for balance, we instead replace the node
	      ;; (to preserve identity) with that on its right, and
	      ;; insert back the previous top key-value mapping.
	      (let ((key (node-key node))
		    (value (node-value node)))
		(copy-node node right)
		(insert i node key value)
		(values left node)))))))

(defmethod divide/list ((i <binary-tree>) node)
  (if (empty-p i node)
      '()
      (let ((left (left node))
            (right (right node)))
        (setf (left node) (empty i)
              (right node) (empty i))
        (append (list node)
                (unless (empty-p i left) (list left))
                (unless (empty-p i right) (list right))))))


;;; Methods used for balancing trees

(defmethod rotate-node-right ((node binary-tree-node))
  ;; (LL2 C:KL LR2) N:K R1 ==> (LL2 N:KL (LR2 C:K R1))
  (let ((child (left node)))
    (rotatef (node-key child) (node-key node))
    (rotatef (node-value child) (node-value node))
    (rotatef (left node) (left child) (right child) (right node)))
  (values))

(defmethod rotate-node-left ((node binary-tree-node))
  ;; L1 N:K (RL2 C:KR RR2) ==> (L1 C:K RL2) N:KR RR2
  (let ((child (right node)))
    (rotatef (node-key child) (node-key node))
    (rotatef (node-value child) (node-value node))
    (rotatef (right node) (right child) (left child) (left node)))
  (values))


;;; Self-balanced trees

(defmethod drop :after ((i <post-self-balanced-binary-tree>) node key)
  (declare (ignore key))
  (balance-node i node))

(defmethod insert :after ((i <post-self-balanced-binary-tree>) node key value)
  (declare (ignore key value))
  (balance-node i node))

;;; Trees that maintain a record of their height

(defmethod copy-node :after ((destination-node heighted-binary-tree-node)
			     (origin-node heighted-binary-tree-node))
  (setf (node-height destination-node) (node-height origin-node))
  (values))

(defmethod update-height ((node heighted-binary-tree-node))
  (setf (node-height node)
        (1+ (max (node-height (left node))
                 (node-height (right node))))))

(defmethod rotate-node-right :after ((node heighted-binary-tree-node))
  (update-height (right node))
  (update-height node))

(defmethod rotate-node-left :after ((node heighted-binary-tree-node))
  (update-height (left node))
  (update-height node))

;;; AVL trees

(defmethod node-class ((i <avl-tree>))
  'avl-tree-node)

(defmethod balance-node ((i <avl-tree>) (node empty-object))
  (values))

(defmethod balance-node ((i <avl-tree>) (node avl-tree-node))
  (ecase (node-balance node)
    ((-1 0 1) ;; already balanced, just update height
     (update-height node))
    ((-2)
     (ecase (node-balance (left node))
       ((-1 0))
       ((1)
        (rotate-node-left (left node))))
     (rotate-node-right node))
    ((2)
     (ecase (node-balance (right node))
       ((-1)
        (rotate-node-right (right node)))
       ((0 1)))
     (rotate-node-left node))))
