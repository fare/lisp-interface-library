;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; mapping of keys to values -- part common to pure and stateful

#+xcvb (module (:depends-on ("interface/base")))

(in-package :interface)

(define-interface <map> (<sizable> <emptyable>) ()
  (:abstract)
  (:generic lookup (<map> map key) (:in 1) (:values value foundp)
   (:documentation "Lookup what map associates to a key,
return two values, the associated value and
a boolean that is true iff an association was found"))
  (:generic
   first-key-value (<map> map) (:in 1) (:values key value foundp)
   (:documentation "Return three values:
1- a key,
2- a value, and
3- a boolean a boolean indicating whether the map was already empty.
What first means here may depend on the particular map interface,
but generally means the element most easily accessible;
it is also the first (leftmost) key and value as used by fold-left and fold-right."))
  (:generic
   fold-left (<map> map f seed) (:in 1) (:values value)
   (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons (pure),
or as by repeatedly deconstructing it as by decons
except without actually modifying the map (stateful),
yielding association k_1 v_1 .. k_n v_n, and computing
  (f (f ... (f (f seed k_1 v_1) k2 v_2) ... k_n-1 v_n-1) k_n v_n)"))
  (:generic
   fold-right (<map> map f seed) (:in 1) (:values value)
   (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons (pure),
or as by repeatedly deconstructing it as by decons
except without actually modifying the map (stateful),
yielding association k_1 v_1 .. k_n v_n, and computing
  (f k_1 v_1 (f k2 v_2 (f ... (f k_n-1 v_n-1 (f k_n v_n seed))...)))"))
  (:generic map-alist (<map> map) (:in 1) (:values alist)
   (:documentation "Convert a map of given interface to an alist"))
  (:generic alist-map (<map> alist) (:values map) (:out 0)
   (:documentation "Convert an alist to a map of given interface")))

(defgeneric key-interface (<map>)
  (:documentation "Interface for the type of keys of a map"))
(defgeneric value-interface (<map>)
  (:documentation "Interface for the type of values of a map"))

;;; Simple Mixins
(define-interface map-fold-right-from-fold-left (<map>) ()
  (:abstract)
  (:method fold-right (map fun seed)
    (funcall
     (fold-left
      map
      #'(lambda (f k v) #'(lambda (acc) (funcall f (funcall fun k v acc))))
      #'identity)
     seed)))

(define-interface map-for-each-from-fold-left (<map>) ()
  (:abstract)
  (:method for-each (map fun)
    (fold-left
     map
     #'(lambda (s k v) (declare (ignore s)) (funcall fun k v))
     nil)
    (values)))

(define-interface map-size-from-fold-left (<map>) ()
  (:abstract)
  (:method size (map)
    (fold-left map #'(lambda (x k v) (declare (ignore k v)) (1+ x)) 0)))

(define-interface map-cheap-size (<sizable>) ()
  (:abstract)
  (:method size<=n-p (map n)
    (<= (size map) n)))
