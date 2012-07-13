;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping of keys to values

#+xcvb (module (:depends-on ("interface/interface" "pure/package")))

(in-package :pure)

(defmethod check-invariant ((i map-simple-empty) (m null) &key)
  m)

(defmethod empty ((i map-simple-empty))
  '())

(defmethod empty-p ((i map-simple-empty) map)
  (null map))

(defmethod decons ((i map-simple-decons) map)
  (multiple-value-bind (k v f) (first-key-value i map)
    (if f
        (values (drop i map k) k v f)
        (values map nil nil nil))))

(defmethod update-key ((i map-simple-update-key) map key fun)
  (multiple-value-bind (value foundp) (lookup i map key)
   (multiple-value-bind (new-value new-foundp) (funcall fun value foundp)
     (cond
       (new-foundp
        (insert i map key new-value))
       (foundp
        (drop i map key))
       (t
        map)))))

(defmethod join ((i map-simple-join) map1 map2)
  (fold-left i map1 #'(lambda (m k v) (insert i m k v)) map2))

(defmethod join/list ((i map-simple-join/list) maplist)
  (reduce #'join maplist :from-end t))

(defmethod divide/list ((i map-simple-divide/list) map)
  (cond
    ((null map) '())
    ((null (cdr map)) (list map))
    (t (multiple-value-list (divide i map)))))

(defmethod map/2 ((i map-simple-map/2) fun map1 map2)
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

(defmethod fold-right ((i map-simple-fold-right) map fun seed)
  (funcall
   (fold-left
    i map
    #'(lambda (f k v) #'(lambda (acc) (funcall f (funcall fun k v acc))))
    #'identity)
   seed))


(defmethod for-each ((i map-simple-for-each) map fun)
  (fold-left
   i map
   #'(lambda (s k v) (declare (ignore s)) (funcall fun k v))
   nil)
  (values))

(defmethod size ((i map-simple-size) map)
  (fold-left i map #'(lambda (x k v) (declare (ignore k v)) (1+ x)) 0))

(defmethod convert ((i2 <map>) (i1 <map>) map1)
  ;; (flow i1 i2 map1 (empty i2))
  (fold-right
   i1 map1
   #'(lambda (k v map2) (insert i2 map2 k v))
   (empty i2)))
