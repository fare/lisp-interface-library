;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure trees

#+xcvb (module (:depends-on ("pure/hash-table-interface")))

(in-package :pure)

(defmethod check-invariant ((i <hash-table>) map &key)
  (check-invariant (hashmap-interface i) map)
  (for-each
   (hashmap-interface i) map
   #'(lambda (hash bucket)
       (declare (ignore hash))
       (check-invariant (bucketmap-interface i) bucket))))

(defmethod empty ((i <hash-table>))
  (empty (hashmap-interface i)))

(defmethod empty-p ((i <hash-table>) map)
  (empty-p (hashmap-interface i) map))

(defmethod lookup ((i <hash-table>) map key)
  (let ((bucket (lookup (hashmap-interface i) map
                        (eq:hash (key-interface i) key))))
    (lookup (bucketmap-interface i) bucket key)))

(defmethod insert ((i <hash-table>) node key value)
  (let ((hash (eq:hash (key-interface i) key)))
    (insert
     (hashmap-interface i) node hash
     (insert (bucketmap-interface i)
             (multiple-value-bind (bucket foundp)
                 (lookup (hashmap-interface i) node hash)
               (if foundp bucket (empty (bucketmap-interface i))))
             key value))))

(defmethod drop ((i <hash-table>) map key)
  (let ((hash (eq:hash (key-interface i) key)))
    (multiple-value-bind (bucket hashfoundp)
        (lookup (hashmap-interface i) map hash)
      (if (null hashfoundp)
          (values map nil nil)
          (multiple-value-bind (new-bucket value foundp)
              (drop (bucketmap-interface i) bucket key)
            (if (null foundp)
                (values map nil nil)
                (values
                 (if (empty-p (bucketmap-interface i) new-bucket)
                     (drop (hashmap-interface i) map hash)
                     (insert (hashmap-interface i) map hash new-bucket))
                 value t)))))))

(defmethod decons ((i <hash-table>) map)
  (multiple-value-bind (hash bucket hashfoundp)
      (first-key-value (hashmap-interface i) map)
    (if (null hashfoundp)
        (values nil map nil nil)
        (multiple-value-bind (foundp new-bucket key value)
            (decons (bucketmap-interface i) bucket)
          (assert foundp)
          (values
           t
           (if (empty-p (bucketmap-interface i) new-bucket)
               (drop (hashmap-interface i) map hash)
               (insert (hashmap-interface i) map hash new-bucket))
           key value)))))

(defmethod first-key-value ((i <hash-table>) map)
  (multiple-value-bind (hash bucket foundp)
      (first-key-value (hashmap-interface i) map)
    (declare (ignore hash))
    (if foundp
        (first-key-value (bucketmap-interface i) bucket)
        (values nil nil nil))))

(defmethod fold-left ((i <hash-table>) node f seed)
  (fold-left (hashmap-interface i) node
             #'(lambda (a h bucket)
                 (declare (ignore h))
                 (fold-left (bucketmap-interface i) bucket f a))
             seed))

(defmethod fold-right ((i <hash-table>) node f seed)
  (fold-right (hashmap-interface i) node
              #'(lambda (h bucket a)
                  (declare (ignore h))
                  (fold-right (bucketmap-interface i) bucket f a))
              seed))

(defmethod for-each ((i <hash-table>) map f)
  (for-each
   (hashmap-interface i) map
   #'(lambda (hash bucket)
       (declare (ignore hash))
       (for-each (bucketmap-interface i) bucket f))))

(defmethod divide ((i <hash-table>) map)
  (if (empty-p (hashmap-interface i) map)
      (values nil nil)
      (multiple-value-bind (a b) (divide (hashmap-interface i) map)
        (if (empty-p (hashmap-interface i) b)
            (multiple-value-bind (hash bucket)
                (first-key-value (hashmap-interface i) a)
              (multiple-value-bind (x y) (divide (bucketmap-interface i) bucket)
                (if (empty-p (bucketmap-interface i) y)
                    (values a b)
                    (values (insert (hashmap-interface i) b hash x)
                            (insert (hashmap-interface i) b hash y)))))
            (values a b)))))

(defmethod divide/list ((i <hash-table>) node)
  (let ((list (divide/list (hashmap-interface i) node)))
    (if (cdr list)
        list
        (multiple-value-bind (a b) (divide i node)
          (cond
            ((empty-p (hashmap-interface i) a)
             nil)
            ((empty-p (hashmap-interface i) b)
             (list a))
            (t
             (list a b)))))))

(defmethod size ((i <hash-table>) map)
  (fold-left (hashmap-interface i) map
             #'(lambda (acc hash bucket) (declare (ignore hash))
                       (+ acc (size (bucketmap-interface i) bucket)))
             0))
