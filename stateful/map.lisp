;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful mapping of keys to values

#+xcvb (module (:depends-on ("stateful/map-interface" "pure/alisp")))

(in-package :stateful)

;;; map-simple-empty
(defclass empty-object () ())
(defun make-empty-object ()
  (make-instance 'empty-object))
(defmethod check-invariant ((i map-simple-empty) (m empty-object) &key)
  m)
(defmethod empty ((i map-simple-empty))
  (make-empty-object))
(defmethod empty-p ((i map-simple-empty) map)
  (typep map 'empty-object))
(defmethod empty! ((i map-simple-empty) map)
  (change-class map 'empty-object)
  (values))

;;; map-simple-decons
(defmethod decons ((i map-simple-decons) map)
  (multiple-value-bind (k v f) (first-key-value i map)
    (cond
      (f
       (drop i map k)
       (values t k v))
      (t
       (values nil nil nil)))))

;;; map-simple-update-key
(defmethod update-key ((i map-simple-update-key) map key fun)
  (multiple-value-bind (value foundp) (lookup i map key)
   (multiple-value-bind (new-value new-foundp) (funcall fun value foundp)
     (cond
       (new-foundp
        (insert i map key new-value))
       (foundp
        (drop i map key)
        (values))
       (t
        (values))))))

;;; map-simple-join
(defmethod join ((i map-simple-join) map1 map2)
  (for-each i map2 #'(lambda (k v) (unless (lookup i map1 k) (insert i map1 k v))))
  map1)

(defmethod join/list ((i map-simple-join/list) maplist)
  (if maplist
      (reduce #'join maplist)
      (empty i)))

(defmethod divide/list ((i map-divide/list-from-divide) map)
  (cond
    ((empty-p i map) '())
    ((size<=n-p i map 1) (list map))
    (t (multiple-value-bind (map2 map) (divide i map) (list map map2)))))

(defmethod map/2 ((i map-simple-map/2) fun map1 map2)
  (for-each i map1
            #'(lambda (k v1)
                (multiple-value-bind (v2 f2) (lookup i map2 k)
                  (multiple-value-bind (v f) (funcall fun k v1 t v2 f2)
                    (if f
                        (insert i map1 k v)
                        (drop i map1 k))))))
  (for-each i map2
            #'(lambda (k v2)
                (multiple-value-bind (v1 f1) (lookup i map1 k)
                  (declare (ignore v1))
                  (unless f1
                    (multiple-value-bind (v f) (funcall fun k nil nil v2 t)
                      (when f
                        (insert i map1 k v)))))))
  map1)

;;; Stateful maps as founts: trivial! BEWARE: this iterator empties the map as it goes.
(defmethod iterator ((<map> <map>) map)
  (declare (ignorable <map>))
  map)
(defmethod next ((<map> <map>) map)
  (multiple-value-bind (f k v) (decons <map> map)
    (if f
        (values k v)
        (error 'end-of-iteration))))

;;; Stateful maps as sinks: trivial!
(defmethod collector ((<map> <map>) map)
  (declare (ignorable <map>))
  map)
(defmethod collect ((<map> <map>) map &rest values)
  (destructuring-bind (key value) values
    (insert <map> map key value)))
(defmethod result ((<map> <map>) map)
  (declare (ignorable <map>))
  map)

;;; Converting a map to another one... may destroy the original one
(defmethod convert ((i2 <map>) (i1 <map>) map1)
  (let ((map2 (empty i2)))
    (flow i1 i2 map1 map2)
    map2))

(defmethod fold-left ((i map-fold-left-from-for-each) map f seed)
  (for-each i map #'(lambda (key value) (setf seed (funcall f seed key value))))
  seed)

(defmethod first-key-value ((i map-first-key-value-from-for-each) map)
  (block nil
    (for-each i map #'(lambda (key value) (return (values key value t))))
    (values nil nil nil)))

(defmethod divide ((i map-divide-from-for-each) map)
  (let ((map2 (empty i))
        (keep t))
    (for-each i map
              #'(lambda (k v)
                  (unless keep
                    (drop i map k)
                    (insert i map2 k v))
                  (setf keep (not keep))))
    (values map2 map)))
