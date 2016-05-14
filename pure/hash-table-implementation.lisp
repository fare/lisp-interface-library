;;;;; Pure hash-tables (implementation)

(uiop:define-package :lil/pure/hash-table-implementation
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/pure/hash-table))
(in-package :lil/pure/hash-table-implementation)

(defmethod check-invariant ((i <hash-table>) map &key)
  (check-invariant (hashmap-interface i) map)
  (for-each*
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
                        (hash (key-interface i) key))))
    (lookup (bucketmap-interface i) bucket key)))

(defmethod insert ((i <hash-table>) map key value)
  (let ((hash (hash (key-interface i) key)))
    (insert
     (hashmap-interface i) map hash
     (insert (bucketmap-interface i)
             (multiple-value-bind (bucket foundp)
                 (lookup (hashmap-interface i) map hash)
               (if foundp bucket (empty (bucketmap-interface i))))
             key value))))

(defmethod drop ((i <hash-table>) map key)
  (let ((hash (hash (key-interface i) key)))
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

(defmethod monoid-fold* ((i <hash-table>) <monoid> map f)
  (monoid-fold*
   (hashmap-interface i) <monoid> map
   #'(lambda (hash bucket)
       (declare (ignore hash))
       (monoid-fold* (bucketmap-interface i) <monoid> bucket f))))

(defmethod fold-left* ((i <hash-table>) map f seed)
  (fold-left* (hashmap-interface i) map
             #'(lambda (a h bucket)
                 (declare (ignore h))
                 (fold-left* (bucketmap-interface i) bucket f a))
             seed))

(defmethod fold-right* ((i <hash-table>) map f seed)
  (fold-right* (hashmap-interface i) map
	       #'(lambda (h bucket a)
		   (declare (ignore h))
		   (fold-right* (bucketmap-interface i) bucket f a))
	       seed))

(defmethod for-each* ((i <hash-table>) map f)
  (for-each*
   (hashmap-interface i) map
   #'(lambda (hash bucket)
       (declare (ignore hash))
       (for-each* (bucketmap-interface i) bucket f))))

(defmethod divide ((i <hash-table>) map)
  (if (empty-p (hashmap-interface i) map)
      (values nil nil)
      (multiple-value-bind (a b) (divide (hashmap-interface i) map)
        (if (empty-p (hashmap-interface i) a) ;; a is empty, b has only one bucket
            (multiple-value-bind (hash bucket)
                (first-key-value (hashmap-interface i) b)
              (multiple-value-bind (x y) (divide (bucketmap-interface i) bucket)
                (if (empty-p (bucketmap-interface i) x)
                    (values a b) ;; a is empty, b has one bucket with one entry
                    (values (insert (hashmap-interface i) a hash x) ;; a is empty, b has one bucket divisible into x and y
                            (insert (hashmap-interface i) a hash y)))))
            (values a b))))) ;; both a and b have buckets

(defmethod divide/list ((i <hash-table>) map)
  (let ((list (divide/list (hashmap-interface i) map)))
    (if (or (null list) (cdr list)) list
        (multiple-value-bind (hash bucket) (first-key-value (hashmap-interface i) map)
          (mapcar #'(lambda (b) () (insert (hashmap-interface i) map hash b))
                  (divide/list (bucketmap-interface i) bucket))))))

(defmethod singleton ((i <hash-table>) pair)
  (let* ((key (car pair))
         (value (cdr pair))
         (hash (hash (key-interface i) key)))
    (singleton (hashmap-interface i)
               (cons hash (singleton (bucketmap-interface i) (cons key value))))))

(defmethod size ((i <hash-table>) map)
  (fold-left* (hashmap-interface i) map
	      #'(lambda (acc hash bucket) (declare (ignore hash))
		  (+ acc (size (bucketmap-interface i) bucket)))
	      0))
