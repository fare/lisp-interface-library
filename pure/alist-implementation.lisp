;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map implementation: alists.

(uiop:define-package :lil/pure/alist-implementation
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base
        :lil/interface/eq
        :lil/interface/group)
  (:use-reexport
   :lil/pure/alist))
(in-package :lil/pure/alist-implementation)

(define-interface-methods (i <alist>)

(:method> check-invariant (map &key)
  (loop :for ((key . nil) . rest) :on map :do
    (assert (not (member key rest
			 :key 'car
			 :test (eq-function (key-interface))))
	    () "Key ~S is present twice in alist ~S" key map)))

(:method> lookup (map key)
  (if (null map)
      (values nil nil)
      (let ((pair (assoc key map :test (eq-function (key-interface)))))
	(if pair
	    (values (cdr pair) t)
	    (values nil nil)))))

(:method> insert (map key value)
  (acons key value (drop map key)))

(:method> drop (map key)
  (if (null map)
      (values nil nil nil)
      (multiple-value-bind (v f) (lookup map key)
        (if f
            (values (remove key map :key 'car :test (eq-function (key-interface))) v t)
            (values map nil nil)))))

(:method> first-key-value (map)
  (values (caar map) (cdar map) (not (null map))))

(:method> fold-left* (map f seed)
  (reduce #'(lambda (acc pair) (funcall f acc (car pair) (cdr pair)))
          map :initial-value seed))

(:method> fold-right* (map f seed)
  (reduce #'(lambda (pair acc) (funcall f (car pair) (cdr pair) acc))
          map :initial-value seed :from-end t))

(:method> for-each* (map f)
  (loop :for (key . val) :in map :do (funcall f key val))
  (values))

(:method> divide (map)
  (let* ((l (length map))
         (l1 (floor l 2)))
    (values (subseq map 0 l1) (nthcdr l1 map))))

(:method> size (map)
  (length map))

;; alists as monoid: useful for folding into an alist ---should we generalize that to maps?
(:method> op (x y)
  (join x y))

(:method> op/list (list)
  (join/list list))

(:method> id ()
  (empty)))

(defmethod map-alist ((<i> lil/interface/map:<map>) map)
  (fold-right* <i> map #'acons '()))

(defmethod convert ((<to> <alist>) (<from> lil/interface/map:<map>) map)
  (declare (ignorable <to>))
  (map-alist <from> map))

(defmethod alist-map ((<map> lil/interface/map:<map>) map)
  (convert <map> <alist> map))
