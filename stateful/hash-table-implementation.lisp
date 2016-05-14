;;;;; Stateful hash-tables (implementation)

(uiop:define-package :lil/stateful/hash-table-implementation
  (:use :closer-common-lisp
        :lil/core
        :lil/interface/base
        :lil/stateful/map-implementation)
  (:use-reexport
   :lil/stateful/hash-table))
(in-package :lil/stateful/hash-table-implementation)

(defun normalize-eq-function (x)
  (cond
    ((member x '(eq eql equal equalp)) x)
    ((eq x #'eq) 'eq)
    ((eq x #'eql) 'eql)
    ((eq x #'equal) 'equal)
    ((eq x #'equalp) 'equalp)
    ((eq x (load-time-value (hash-table-test (make-hash-table :test 'eq)))) 'eq)
    ((eq x (load-time-value (hash-table-test (make-hash-table :test 'eql)))) 'eql)
    ((eq x (load-time-value (hash-table-test (make-hash-table :test 'equal)))) 'equal)
    ((eq x (load-time-value (hash-table-test (make-hash-table :test 'equalp)))) 'equalp)))

(defun same-eq-function-p (x y)
  (let ((x (normalize-eq-function x))
        (y (normalize-eq-function y)))
    (and x y (eq x y))))

(defmethod check-invariant ((i <hash-table>) map &key)
  (check-type map hash-table)
  (assert (normalize-eq-function (eq-function (key-interface i))))
  (assert (same-eq-function-p (eq-function (key-interface i)) (hash-table-test map))))

(defmethod empty ((i <hash-table>))
  (make-hash-table :test (normalize-eq-function (eq-function (key-interface i)))))

(defmethod empty-p ((i <hash-table>) map)
  (= 0 (hash-table-count map)))

(defmethod lookup ((i <hash-table>) map key)
  (gethash key map))

(defmethod insert ((i <hash-table>) map key value)
  (setf (gethash key map) value)
  (values))

(defmethod drop ((i <hash-table>) map key)
  (multiple-value-bind (value foundp) (gethash key map)
    (remhash key map)
    (values value foundp)))

(defmethod for-each* ((i <hash-table>) map f)
  (loop :for key :being :the :hash-keys :of map :using (:hash-value value)
    :do (funcall f key value)))

(defmethod size ((i <hash-table>) map)
  (hash-table-count map))
