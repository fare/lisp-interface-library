;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; mapping of keys to values -- part common to pure and stateful

(uiop:define-package :lil/interface/map
  (:use :closer-common-lisp
   :lil/interface/definition :lil/interface/base :lil/interface/group
   :lil/interface/collection :lil/interface/fold)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:<map> ;; to be shadowed by pure and stateful packages.
   #:lookup #:first-key-value #:map-alist #:alist-map
   #:node-class
   #:node #:node-key #:node-value #:left #:right #:node-balance #:node-height
   #:locate #:node-key-value #:leftmost-node #:rightmost-node #:leftmost #:rightmost #:leftmost-entry #:rightmost-entry
   #:key-interface #:value-interface
   #:<map-foldable-from-*>
   #:<map-for-each*-from-fold-left*>
   #:<map-fold-right*-from-fold-left*>
   #:<map-has-key-p-from-lookup>
   #:<map-monoid-fold*-from-fold-left*>))

(in-package :lil/interface/map)

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
    (monoid-fold* <monoid> map (compose fun #'cons)))
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

(define-interface <map-fold-right*-from-fold-left*> (<foldable>) ()
  (:abstract)
  (:method> fold-right* (map fun seed)
    (funcall
     (fold-left*
      map
      #'(lambda (f k v) #'(lambda (acc) (funcall f (funcall fun k v acc))))
      #'identity)
     seed)))

(define-interface <map-fold-left*-from-fold-right*> (<foldable>) ()
  (:abstract)
  (:method> fold-left* (map fun seed)
    (funcall
     (fold-right*
      map
      #'(lambda (k v f) #'(lambda (acc) (funcall f (funcall fun acc k v))))
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

(define-interface <map-has-key-p-from-lookup> (<map>) ()
  (:method> has-key-p (collection key)
     (nth-value 2 (lookup collection key))))

(define-interface <map-monoid-fold*-from-fold-left*> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold* (<monoid> map fun)
    (fold-left*
     map
     #'(lambda (acc k v) (op <monoid> acc (funcall fun k v)))
     (id <monoid>))))
