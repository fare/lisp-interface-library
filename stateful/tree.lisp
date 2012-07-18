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
                (if (empty-p i (right node))
                    (empty! i node)
                    (copy-node node (right node))))
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

;;; TODO: implement proper balancing for insert and drop
(defmethod node ((i <avl-tree>) &key left right key value)
  (flet ((mk (&key left right key value)
           (let ((lh (node-height left))
                 (rh (node-height right)))
             (assert (member (- rh lh) '(-1 0 1)))
             (make-instance 'avl-tree-node
                            :key key :value value
                            :left left :right right
                            :height (1+ (max lh rh))))))
    (ecase (- (node-height right) (node-height left))
      ((-1 0 1) (mk :key key :value value :left left :right right))
      ((-2)
       (ecase (node-balance left)
         ((-1 0)
          ;; -1: LL rebalance:
          ;; (LL2 KL LR1) K R1 ==> (LL2 KL (LR1 K R1))
          ;; 0: left rebalance during deletion
          ;; (LL2 KL LR2) K R1 ==> (LL2 KL (LR2 K R1))
          (mk :left (left left)
              :key (node-key left) :value (node-value left)
              :right (mk :key key :value value :left (right left) :right right)))
         ((1)
          ;; LR rebalance:
          ;; (LL1 KL (LRL21 KLR LRR21)) K R1 ==> (LL1 KL LRL21) KLR (LRR21 K R1)
          (mk :left (mk :left (left left)
                        :key (node-key left) :value (node-value left)
                        :right (left (right left)))
              :key (node-key (right left)) :value (node-value (right left))
              :right (mk :left (right (right left))
                         :key key :value value
                         :right right)))))
      ((2)
       (ecase (node-balance right)
         ((-1)
          ;; RL rebalance:
          ;; L1 K ((RLL21 KRL RLR21) KR RR1) ==> (L1 K RLL21) KRL (RLR21 KR RR1)
          (mk :left (mk :left left
                        :key key :value value
                        :right (left (left right)))
              :key (node-key (left right)) :value (node-value (left right))
              :right (mk :left (right (left right))
                         :key (node-key right) :value (node-value right)
                         :right (right right))))
         ((0 1)
          ;; -1: RR rebalance:
          ;; L1 K (RL1 KR RR2) ==> (L1 K RL1) KR RR2
          ;; 0: right rebalance during deletion
          ;; L1 K (RL2 KR RR2) ==> (L1 K RL2) KR RR2
          (mk :left (mk :left left
                        :key key :value value
                        :right (left right))
              :key (node-key right) :value (node-value right)
              :right (right right))))))))
