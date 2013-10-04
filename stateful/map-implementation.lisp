;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful mapping of keys to values (implementation)

(uiop:define-package :lil/stateful/map-implementation
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base
        :lil/interface/group
        :lil/interface/fold)
  (:use-reexport :lil/stateful/map))
(in-package :lil/stateful/map-implementation)

;;; <map-empty-is-empty-object>
(define-interface-methods (<i> <map-empty-is-empty-object>)
  (:method> empty! (map)
    (change-class map 'empty-object)
    (values))
  (:method> node-key-value ((m empty-object))
    (values nil nil nil)))

;;; <map-decons-from-first-key-value-drop>
(defmethod decons ((i <map-decons-from-first-key-value-drop>) map)
  (multiple-value-bind (k v f) (first-key-value i map)
    (cond
      (f
       (drop i map k)
       (values t k v))
      (t
       (values nil nil nil)))))

;;; <map-update-key-from-lookup-insert-drop>
(defmethod update-key ((i <map-update-key-from-lookup-insert-drop>) map key fun)
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

;;; <map-join-from-for-each*-lookup-insert>
(defmethod join ((<i> <map-join-from-for-each*-lookup-insert>) map1 map2)
  (for-each* <i> map2
	     #'(lambda (k v)
		 (unless (lookup <i> map1 k)
		   (insert <i> map1 k v))))
  (values))

(defmethod join/list ((<i> <map-join-from-for-each*-lookup-insert>) maplist)
  (with-interface (<i> <map>)
    (if maplist
	(reduce #'join maplist)
	(empty))))

(defmethod divide/list ((<i> <map-divide/list-from-divide>) map)
  (cond
    ((empty-p <i> map) '())
    ((size<=n-p <i> map 1) (list map))
    (t (multiple-value-bind (map2 map) (divide <i> map) (list map map2)))))

(defmethod map/2 ((<i> <map-map/2-from-for-each*-lookup-insert-drop>) fun map1 map2)
  (with-interface (<i> <map>)
    (for-each* map1
	       #'(lambda (k v1)
		   (multiple-value-bind (v2 f2) (lookup map2 k)
		     (multiple-value-bind (v f) (funcall fun k v1 t v2 f2)
		       (if f
			   (insert map1 k v)
			   (drop map1 k))))))
    (for-each* map2
	       #'(lambda (k v2)
		   (multiple-value-bind (v1 f1) (lookup map1 k)
		     (declare (ignore v1))
		     (unless f1
		       (multiple-value-bind (v f) (funcall fun k nil nil v2 t)
			 (when f
			   (insert map1 k v)))))))
    map1))

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
(defmethod convert ((<to> <map>) (<from> lil/interface/map:<map>) frommap)
  (let ((tomap (empty <to>)))
    (for-each* <from> frommap #'(lambda (k v) (insert <to> tomap k v)))
    tomap))

(defmethod fold-left* ((<i> <map-fold-left*-from-for-each*>) map f seed)
  (for-each* <i> map #'(lambda (key value) (setf seed (funcall f seed key value))))
  seed)

(defmethod first-key-value ((<i> <map-first-key-value-from-for-each*>) map)
  (block nil
    (for-each* <i> map #'(lambda (key value) (return (values key value t))))
    (values nil nil nil)))

(defmethod divide ((<i> <map-divide-from-for-each*>) map)
  (let ((map2 (empty <i>))
        (keep t))
    (for-each* <i> map
	       #'(lambda (k v)
		   (unless keep
		     (drop <i> map k)
		     (insert <i> map2 k v))
		   (setf keep (not keep))))
    (values map2 map)))
