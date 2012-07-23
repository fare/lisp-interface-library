;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; mapping of keys to values -- part common to pure and stateful

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

(define-interface <map> (<emptyable>)
  ()
  (:generic lookup (:in 1) (<map> map key)
   (:documentation "Lookup what map associates to a key,
return two values, the associated value and
a boolean that is true iff an association was found"))
  (:generic
   first-key-value (:in 1) (<map> map)
   (:documentation "Return three values:
1- a key,
2- a value, and
3- a boolean a boolean indicating whether the map was already empty.
What first means here may depend on the particular map interface,
but generally means the element most easily accessible;
it is also the first (leftmost) key and value as used by fold-left and fold-right."))
  (:generic
   fold-left (:in 1) (<map> map f seed)
   (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons (pure),
or as by repeatedly deconstructing it as by decons
except without actually modifying the map (stateful),
yielding association k_1 v_1 .. k_n v_n, and computing
  (f (f ... (f (f seed k_1 v_1) k2 v_2) ... k_n-1 v_n-1) k_n v_n)"))
  (:generic
   fold-right (:in 1) (<map> map f seed)
   (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons (pure),
or as by repeatedly deconstructing it as by decons
except without actually modifying the map (stateful),
yielding association k_1 v_1 .. k_n v_n, and computing
  (f k_1 v_1 (f k2 v_2 (f ... (f k_n-1 v_n-1 (f k_n v_n seed))...)))"))
  (:generic map-alist (:in 1) (<map> map)
   (:documentation "Convert a map of given interface to an alist"))
  (:generic alist-map (:in 1) (<map> map)
   (:documentation "Convert an alist to a map of given interface")))


;;; Simple Mixins
(defclass map-fold-right-from-fold-left () ())

(defmethod fold-right ((i map-fold-right-from-fold-left) map fun seed)
  (funcall
   (fold-left
    i map
    #'(lambda (f k v) #'(lambda (acc) (funcall f (funcall fun k v acc))))
    #'identity)
   seed))

(defclass map-for-each-from-fold-left () ())

(defmethod for-each ((i map-for-each-from-fold-left) map fun)
  (fold-left
   i map
   #'(lambda (s k v) (declare (ignore s)) (funcall fun k v))
   nil)
  (values))

(defclass map-size-from-fold-left () ())

(defmethod size ((i map-size-from-fold-left) map)
  (fold-left i map #'(lambda (x k v) (declare (ignore k v)) (1+ x)) 0))

(define-interface map-cheap-size (<interface>) ())

(defmethod size<=n-p ((i map-cheap-size) map n)
  (<= (size i map) n))
