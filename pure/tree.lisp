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
  (if (empty-p i node)
      (node i :key key :value value)
      (ecase (compare (key-interface i) key (node-key node))
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
        (ecase (compare (key-interface i) key k)
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
