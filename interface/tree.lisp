;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("interface/tree-interface")))

(in-package :interface)

(defmethod node-class ((i <binary-tree>))
  'binary-tree-node)

(defmethod check-invariant ((i <binary-tree>) (node binary-branch) &key
                            lower (lowerp lower) upper (upperp upper))
  (typep node (node-class i))
  (let ((key (node-key node)))
    (when lowerp
      (assert (order:order< i lower key)))
    (when upperp
      (assert (order:order< i key upper)))
    (when (left node)
      (check-invariant i (left node) :lowerp lowerp :lower lower :upperp t :upper key))
    (when (right node)
      (check-invariant i (right node) :lowerp t :lower key :upperp upperp :upper upper))))

;;(defmethod node ((i <tree>) &rest keys &key #+sbcl &allow-other-keys)
;;  (apply #'make (node-interface i) keys))
;;(defmethod compare-key ((i <map>) key1 key2)
;;  (compare (key-interface i) key1 key2))

(defmethod locate ((i <binary-tree>) node key path)
  (ecase (order:compare i key (node-key node)) ;; (compare-key i key (node-key node))
    (0 (values node path))
    (-1 (locate i (left node) key (cons 'left path)))
    (1 (locate i (right node) key (cons 'right path)))))

(defmethod lookup ((i <binary-tree>) node key)
  (if (empty-p i node)
      (values nil nil)
      (ecase (order:compare i key (node-key node)) ;; (compare-key i key (node-key node))
        (0 (values (node-value node) t))
        (-1 (lookup i (left node) key))
        (1 (lookup i (right node) key)))))

(defmethod first-key-value ((i <binary-tree>) map)
  "Return key and value with the least key"
  (leftmost i map))

(defmethod fold-left ((i <binary-tree>) node f seed)
  (if (empty-p i node)
      seed
      (fold-left i (right node) f
                      (funcall f
                               (fold-left i (left node) f seed)
                               (node-key node) (node-value node)))))

(defmethod fold-right ((i <binary-tree>) node f seed)
  (if (empty-p i node)
      seed
      (fold-right i (left node) f
                       (funcall f
                                (node-key node) (node-value node)
                                (fold-right i (right node) f seed)))))

(defmethod for-each ((i <binary-tree>) node f)
  (unless (empty-p i node)
    (for-each i (left node) f)
    (funcall f (node-key node) (node-value node))
    (for-each i (right node) f))
  (values))

(defmethod node-key-value ((i <binary-tree>) (node binary-tree-node))
   (values (node-key node) (node-value node) t))

(defmethod leftmost-node ((i <binary-tree>) node)
  (cond
    ((empty-p i node) node)
    ((empty-p i (left node)) node)
    (t (leftmost-node i (left node)))))

(defmethod leftmost ((i <binary-tree>) node)
  (node-key-value i (leftmost-node i node)))

(defmethod rightmost-node ((i <binary-tree>) node)
  (cond
    ((empty-p i node) node)
    ((empty-p i (right node)) node)
    (t (rightmost-node i (right node)))))

(defmethod rightmost ((i <binary-tree>) node)
  (node-key-value i (rightmost-node i node)))

(defmethod node-class ((i <avl-tree>))
  'avl-tree-node)

(defmethod node-height ((node null))
  0)

(defmethod node-balance ((node null))
  0)

(defmethod node-height ((node empty-object))
  0)

(defmethod node-balance ((node empty-object))
  0)

(defmethod node-balance ((node avl-tree-node))
  (- (node-height (right node))
     (node-height (left node))))

(defmethod check-invariant :before ((i <avl-tree>) (node avl-tree-node) &key)
  (assert (typep (node-height node)
                 `(integer 1 ,most-positive-fixnum)))
  (assert (= (node-height node)
             (1+ (max (node-height (left node))
                      (node-height (right node))))))
  (assert (member (node-balance node) '(-1 0 1))))

#| Minimum number of nodes in a tree of height n (maximum is 2^n-1)
(fmemo:define-memo-function f (n)
  (cond ((zerop n) 0)
        ((= n 1) 1)
        (t (+ 1 (f (1- n)) (f (- n 2))))))
It's a variant of the fibonacci function,
and it grows exponentially like phi^n when n is big.
This ensures that even in the worst-case scenario,
a balanced tree is logarithmically shallow.

Exercise: prove that the in the above algorithms,
node is always called with branches that are of comparable height...
|#

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

(defmethod print-object ((object binary-tree-node) stream)
  (format stream "#<~S ~S>" (type-of object) (binary-tree-sexp object)))

(defun binary-tree-sexp (tree)
  (etypecase tree
    (null nil)
    (empty-object nil)
    (binary-tree-node
     (list (binary-tree-sexp (left tree))
           (cons (node-key tree) (node-value tree))
           (binary-tree-sexp (right tree))))))
