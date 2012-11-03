;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; mapping of keys to values -- part common to pure and stateful

#+xcvb (module (:depends-on ("interface/base")))

(in-package :interface)

(define-interface <map> (<finite-collection>) ()
  (:abstract)
  (:generic> value-interface () (:values interface)
   (:documentation "Interface for the type of values of a map (codomain)"))
  (:generic> lookup (map key) (:in 1) (:values value foundp)
   (:documentation "Lookup what map associates to a key,
return two values, the associated value and
a boolean that is true iff an association was found"))
  (:generic> first-key-value (map) (:in 1) (:values key value foundp)
   (:documentation "Return three values:
1- a key,
2- a value, and
3- a boolean a boolean indicating whether the map was already empty.
What first means here may depend on the particular map interface,
but generally means the element most easily accessible;
it is also the first (leftmost) key and value as used by fold-left and fold-right."))
  (:generic> map-alist (map) (:in 1) (:values alist)
   (:documentation "Convert a map of given interface to an alist"))
  (:generic> alist-map (alist) (:values map) (:out 0)
   (:documentation "Convert an alist to a map of given interface")))

;;; Simple Mixins
(define-interface <map-foldable-from-*> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold (<monoid> map fun)
    (monoid-fold* map (compose fun #'cons)))
  (:method> fold-left (map fun seed)
    (fold-left* map #'(lambda (a k v) (funcall fun a (cons k v))) seed))
  (:method> fold-right (map fun seed)
    (fold-right* map #'(lambda (k v a) (funcall fun (cons k v) a)) seed))
  (:method> for-each (map fun)
    (for-each* map #'(lambda (k v) (funcall fun (cons k v))))))

#|
(define-interface <unary-function> (<monoid>) ()
  (:singleton)
  (:method> op (a b) (compose a b))
  (:method> op/list (list) (apply 'compose list))
  (:method> id () #'identity))

(define-interface <foldable-fold-left-from-monoid-fold> (<foldable>) ()
  (:abstract)
  (:method> fold-left (map fun seed)
     (funcall
      (monoid-fold*
       <unary-function> map
       #'(lambda (entry) #'(lambda (acc) (funcall (fun acc entry)))))
      seed)))
|#

(define-interface <map-monoid-fold*-from-fold-left*> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold* (<monoid> map fun)
    (fold-left*
     map
     #'(lambda (acc k v) (op <monoid> acc (funcall fun k v)))
     (id <monoid>))))

(define-interface <map-fold-right*-from-fold-left*> (<foldable>) ()
  (:abstract)
  (:method> fold-right* (map fun seed)
    (funcall
     (fold-left*
      map
      #'(lambda (f k v) #'(lambda (acc) (funcall f (funcall fun k v acc))))
      #'identity)
     seed)))

(define-interface <map-fold-right*-from-fold-left*> (<foldable>) ()
  (:abstract)
  (:method> fold-right* (map fun seed)
    (funcall
     (fold-left*
      map
      #'(lambda (f k v) #'(lambda (acc) (funcall f (funcall fun k v acc))))
      #'identity)
     seed)))

(define-interface <map-for-each*-from-fold-left*> (<foldable>) ()
  (:abstract)
  (:method> for-each* (map fun)
    (fold-left*
     map
     #'(lambda (s k v) (declare (ignore s)) (funcall fun k v))
     nil)
    (values)))
