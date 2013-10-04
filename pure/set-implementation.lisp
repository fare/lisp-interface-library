;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Pure Sets and Multisets — implementation

(uiop:define-package :lil/pure/set-implementation
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/pure/set))
(in-package :lil/pure/set-implementation)

;;; set
(defmethod set-union ((<i> <set>) set1 set2)
  (join <i> set1 set2))
(defmethod set-union/list ((<i> <set>) list)
  (join/list <i> list))

(symbol-macrolet ((<b> (base-interface <i>)))

;;; <set*-from-collection>
(defmethod empty ((<i> <set*-from-collection>))
  (empty <b>))
(defmethod empty-p ((<i> <set*-from-collection>) x)
  (empty-p <b> x))
(defmethod empty! ((<i> <set*-from-collection>) x)
  (empty! <b> x))

;;; <set-from-map>
(defmethod singleton ((<i> <set-from-map>) element)
  (singleton <b> (cons element t)))
(defmethod singleton-p ((<i> <set-from-map>) set)
  (singleton-p <b> set))
(defmethod set-union ((<i> <set-from-map>) set1 set2)
  (join <b> set1 set2))
(defmethod set-union/list ((<i> <set-from-map>) list)
  (join/list <b> list))
(defmethod set-intersection ((<i> <set-from-map>) set1 set2)
  (restriction <b> set1 #'(lambda (k) (has-key-p <b> set2 k))))
(defmethod set-intersection/list ((<i> <set-from-map>) list)
  (if list
      (let ((x (first list))
            (r (rest list)))
        (restriction <b> x #'(lambda (k) (every #'(lambda (s) (has-key-p <b> s k)) r))))
      (empty <b>)))
(defmethod set-disjunction ((<i> <set-from-map>) set1 set2)
  (restriction <b> set1 #'(lambda (k) (not (has-key-p <b> set2 k)))))
(defmethod subset-p ((<i> <set-from-map>) set1 set2)
  (block nil
    (for-each* <b> set1 #'(lambda (k v) (declare (ignore v)) (unless (has-key-p <b> set2 k) (return nil))))
    t))
(defmethod proper-subset-p ((<i> <set-from-map>) set1 set2)
  (and (subset-p <i> set1 set2)
       (not (subset-p <i> set2 set1))))

;;; <set-from-multiset>
(defmethod singleton ((<i> <set-from-multiset>) element)
  (singleton <b> element))

(defmethod increase-member-count ((<i> <multiset>) collection key &optional (count 1))
  (check-type count (integer 1 *))
  (update-key <b> collection key #'(lambda (v f) (values (if f (+ v count) count) t))))

(defmethod decrease-member-count ((<i> <multiset>) collection key &optional (count 1))
  (check-type count (integer 1 *))
  (update-key <b> collection key
              #'(lambda (v f)
                  (let ((new-count (and f (- v count))))
                    (typecase new-count
                      ((integer 1 *)
                       (values new-count t))
                      ((eql 0)
                       (values nil nil))
                      (t
                       (error "Existing count not sufficient to decrease-member-count for key ~S by ~S in collection ~S"
                              key count collection)))))))

#|
(define-interface <set*-from-collection> (lil/interface/set:<set*-from-collection> <set*>) ()
  (:method empty! ((i <set*-from-collection>) x) (empty (base-interface i))))

(define-interface <set-from-map> (<set> <set*-from-collection> lil/interface/set:<set-from-map>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method singleton ((<i> <set-from-map>) element)
    (singleton <b> (cons element t))))

(define-interface <multiset-from-map> (<multiset> <set*-from-collection> lil/interface/set:<multiset-from-map>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method singleton ((<i> <multiset-from-map>) element)
    (singleton <b> (cons element 1))))

(define-interface <set-from-multiset> (<set> <set*-from-collection> lil/interface/set:<set-from-multiset>)
  ((base-interface :type <multiset>))
  (:parametric (base-interface)
    (make-interface :base-interface base-interface))
  (:method singleton ((<i> <set-from-multiset>) element)
    (singleton <b> element)))

(define-interface <multiset-from-set> (<multiset> <set*-from-collection> lil/interface/set:<multiset-from-set>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method singleton ((<i> <set-from-multiset>) element)
    (singleton <b> element)))
|#
); symbol-macrolet
