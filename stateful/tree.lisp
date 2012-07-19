;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("stateful/tree-interface")))

(in-package :stateful)

(defmethod node ((i <binary-tree>) &key left right key value)
  (make-instance 'binary-tree-node
                 :key key :value value :left left :right right))

(defmethod insert ((i <binary-tree>) node key value)
  (cond
    ((empty-p i node)
     (change-class node '<binary-tree> :key key :value value)
     (values))
    (t
     (ecase (order:compare i key (node-key node))
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
        (ecase (order:compare i key k)
          (0 (cond
               ((empty-p i (left node))
                (copy-node node (right node)))
               ((empty-p i (right node))
                (copy-node node (left node)))
               (t
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
  (cond
    ((empty-p i node)
     (values (empty i) node))
    ((empty-p i (left node))
     (let ((right (right node)))
       (setf (right node) (empty i))
       (values right node)))
    (t
     (let ((left (left node)))
       (setf (left node) (empty i))
       (values left node)))))

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

(defmethod drop :after ((i <post-self-balanced-binary-tree>) node key)
  (declare (ignore key))
  (balance-node i node))

(defmethod insert :after ((i <post-self-balanced-binary-tree>) node key value)
  (declare (ignore key))
  (balance-node i node))

(defmethod copy-node ((destination-node avl-tree-node) (origin-node avl-tree-node))
  (setf (node-height destination-node) (node-height origin-node))
  (values))

(defmethod balance-node ((i <avl-tree>) (node empty-object))
  (values))

(defmethod update-height ((node avl-tree-node))
  (setf (node-height node)
        (1+ (max (node-height (left node))
                 (node-height (right node))))))

(defmethod rotate-node-right ((node binary-tree-node))
  ;; (LL2 C:KL LR2) N:K R1 ==> (LL2 N:KL (LR2 C:K R1))
  (let ((child (left node)))
    (rotatef (node-key child) (node-key node))
    (rotatef (node-value child) (node-value node))
    (rotatef (left node) (left child) (right child) (right node)))
  (values))

(defmethod rotate-node-right :after ((node avl-tree-node))
  (update-height (right node))
  (update-height node))

(defmethod rotate-node-left ((node avl-tree-node))
  ;; L1 N:K (RL2 C:KR RR2) ==> (L1 C:K RL2) N:KR RR2
  (let ((child (right node)))
    (rotatef (node-key child) (node-key node))
    (rotatef (node-value child) (node-value node))
    (rotatef (right node) (right child) (left child) (left node)))
  (values))

(defmethod rotate-node-left :after ((node avl-tree-node))
  (update-height (left node))
  (update-height node))

(defmethod balance-node ((i <avl-tree>) (node avl-tree-node))
  (ecase (node-balance node)
    ((-1 0 1) (values)) ;; already balanced
    ((-2)
     (ecase (node-balance (left node))
       ((-1 0)
        (rotate-node-right node))
       ((1)
        (rotate-node-right node)
        (rotate-node-left node))))
    ((2)
     (ecase (node-balance (right node))
       ((-1)
        (rotate-node-left node)
        (rotate-node-right node))
       ((0 1)
        (rotate-node-left node))))))
