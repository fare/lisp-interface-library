;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure/tree-interface")))

(in-package :pure)

(defmethod node-class ((i <binary-tree>))
  'binary-tree-node)

(defmethod node ((i <binary-tree>) &key left right key value)
  (make-instance (node-class i)
                 :key key :value value :left left :right right))

(defmethod insert ((i <binary-tree>) node key value)
  (if (null node)
      (node i :key key :value value)
      (ecase (order:compare i key (node-key node))
        (0 (node i :key key :value value ;; (update-node i node :key key :value value)
                 :left (left node) :right (right node)))
        (-1 (node i :key (node-key node) :value (node-value node)
                  :left (insert i (left node) key value) :right (right node)))
        (1 (node i :key (node-key node) :value (node-value node)
                 :left (left node) :right (insert i (right node) key value))))))

(defmethod drop ((i <binary-tree>) node key)
  (if (null node)
      (values nil nil nil)
      (let ((k (node-key node))
            (v (node-value node)))
        (ecase (order:compare i key k)
          (0 (values
              (cond
                ((null (left node)) (right node))
                ((null (right node)) (left node))
                (t
                 (multiple-value-bind (kk vv)
                     (leftmost i (right node))
                   (node i :key kk :value vv
                         :left (left node) :right (drop i (right node) kk)))))
              v t))
          (-1
           (multiple-value-bind (left value foundp) (drop i (left node) key)
                (values (node i :key k :value v
                              :left left :right (right node))
                    value foundp)))
          (1
           (multiple-value-bind (right value foundp) (drop i (right node) key)
               (values (node i :key k :value v
                             :left (left node) :right right)
                       value foundp)))))))

(defmethod divide ((i <binary-tree>) node)
  (if (null node)
      (values nil nil)
      (values (left node) (insert i (right node) (node-key node) (node-value node)))))

(defmethod divide/list ((i <binary-tree>) node)
  (if (null node) '()
      (let* ((rlist (cons (node i :key (node-key node) :value (node-value node))
                          (if (null (right node)) '() (list (right node))))))
        (if (null (left node)) rlist (cons (left node) rlist)))))

(defmethod node-class ((i <avl-tree>))
  'avl-tree-node)

(defmethod node ((i <avl-tree>) &key left right key value)
  (flet ((mk (&key left right key value)
           (let ((lh (node-height left))
                 (rh (node-height right)))
             (assert (member (- rh lh) '(-1 0 1)))
             (make-instance (node-class i)
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
