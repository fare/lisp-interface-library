;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping of keys to values

#+xcvb (module (:depends-on ("pure/map-interface")))

(in-package :pure)

;; <map-empty-is-nil>
(defmethod node-key-value ((<i> <map-empty-is-nil>) (m null))
  (declare (ignorable <i> m))
  (values nil nil nil))

;; <map-decons-from-first-key-value-drop>
(defmethod decons ((<i> <map-decons-from-first-key-value-drop>) map)
  (multiple-value-bind (k v f) (first-key-value <i> map)
    (if f
        (values f (drop <i> map k) k v)
        (values nil map nil nil))))

;; <map-update-key-from-lookup-insert-drop>
(defmethod update-key ((<i> <map-update-key-from-lookup-insert-drop>) map key fun)
  (multiple-value-bind (value foundp) (lookup <i> map key)
   (multiple-value-bind (new-value new-foundp) (funcall fun value foundp)
     (cond
       (new-foundp
        (insert <i> map key new-value))
       (foundp
        (drop <i> map key))
       (t
        map)))))

;; <map-join-from-fold-left-insert>
(defmethod join ((<i> <map-join-from-fold-left-insert>) map1 map2)
  (fold-left <i> map1 #'(lambda (m k v) (insert <i> m k v)) map2))

;; <map-join/list-from-join>
(defmethod join/list ((<i> <map-join/list-from-join>) maplist)
  (reduce #'(lambda (m1 m2) (join <i> m1 m2)) maplist :from-end t))

;; <map-divide/list-from-divide>
(defmethod divide/list ((<i> <map-divide/list-from-divide>) map)
  (cond
    ((null map) '())
    ((empty-p <i> (nth-value 1 (decons <i> map))) (list map))
    (t (multiple-value-list (divide <i> map)))))

;; <map-map/2-from-fold-left-lookup-insert-drop>
(defmethod map/2 ((i <map-map/2-from-fold-left-lookup-insert-drop>) fun map1 map2)
  (labels ((join1 (a k v1)
             (let ((mm (car a))
                   (m2 (cdr a)))
               (multiple-value-bind (v2 f2) (lookup i m2 k)
                 (multiple-value-bind (v f) (funcall fun k v1 t v2 f2)
                   (let ((nmm (if f (insert i mm k v) mm))
                         (nm2 (if f2 (drop i m2 k) m2)))
                     (cons nmm nm2))))))
           (join2 (mm k v2)
             (multiple-value-bind (v f) (funcall fun k nil nil v2 t)
               (if f (insert i mm k v) mm))))
    (destructuring-bind (mm . m2)
        (fold-left i map1 #'join1 (cons (empty i) map2))
      (fold-left i m2 #'join2 mm))))

;;; Functional maps as founts: trivial!
(defmethod iterator ((<map> <map>) map)
  (declare (ignorable <map>))
  map)
(defmethod next ((<map> <map>) map)
  (decons <map> map))

;;; Functional maps as sinks: trivial!
(defmethod collector ((<map> <map>) map)
  (declare (ignorable <map>))
  map)
(defmethod collect ((<map> <map>) map &rest values)
  (destructuring-bind (key value) values
    (insert <map> map key value)))
(defmethod result ((<map> <map>) map)
  (declare (ignorable <map>))
  map)

;;; Converting a map to another one...
(defmethod convert ((i2 <map>) (i1 <map>) map1)
  ;; Using iterators: (flow i1 i2 map1 (empty i2))
  ;; Assuming fold-right preserves any insertion order:
  (fold-right
   i1 map1
   #'(lambda (k v map2) (insert i2 map2 k v))
   (empty i2)))

(defmethod size<=n-p ((<i> <map-size<=n-p-from-decons>) map n)
  (check-type n (integer 0 *))
  (cond
    ((empty-p <i> map) t)
    ((zerop n) nil)
    (t (size<=n-p <i> (nth-value 1 (decons <i> map)) (1- n)))))
