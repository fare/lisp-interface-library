;;;;; Sets and Multisets -- part common to pure and stateful

(uiop:define-package :lil/interface/set
  (:use :closer-common-lisp :lil/core
        :lil/interface/base :lil/interface/order :lil/interface/fold
        :lil/interface/collection :lil/interface/map)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:<set*> #:<set> ;; to be shadowed by pure and stateful packages.
   #:member-p #:set-list #:list-set
   #:<multiset> ;; to be shadowed by pure and stateful packages.
   #:member-count #:multiset-list #:list-multiset
   #:<set*-from-collection>
   #:<set-from-map> #:<multiset-from-map> #:<set-from-multiset> #:<multiset-from-set>))
(in-package :lil/interface/set)

(define-interface <set*> (<finite-collection>) ()
  (:abstract)
  (:method> value-interface () <boolean>)
  (:generic> member-p (element set*) (:in 2) (:values foundp)
   (:documentation "Is a given object member of the set?"))
  (:generic> member-count (element set*) (:in 2) (:values foundp)
   (:documentation "How many times for the element appear in the set*?"))
  (:generic> set-list (set) (:in 1) (:values list)
   (:documentation "Convert a set of given interface to a list"))
  (:generic> list-set (list) (:values map) (:out 0)
   (:documentation "Convert a list to a set of given interface")))

(define-interface <set> (<set*> <foldable-*-from>) () ;; a set is a map from the key interface to boolean
  (:abstract)
  (:method> value-interface () <boolean>)
  (:method> member-count (element set) (boolean-integer (member-p element set))))

(define-interface <multiset> (<set*> <map-foldable-from-*>) () ;; a multiset is a map from the key interface to (positive) integer
  (:abstract)
  (:method> value-interface () <integer>)
  (:method> member-p (element multiset) (plusp (member-count element multiset)))
  (:generic> multiset-list (set) (:in 1) (:values list)
   (:documentation "Convert a multiset of given interface to a list"))
  (:generic> list-multiset (list) (:values map) (:out 0)
   (:documentation "Convert a list to a multiset of given interface")))

(defun cons--t (x) (cons x t))
(defun cons--1 (x) (cons x 1))

(define-interface <set*-from-collection> (<has-base-interface> <set*>)
  ((base-interface :type <map> :initarg :base-interface :reader base-interface))
  (:method key-interface ((<i> <set*-from-collection>)) (key-interface (base-interface <i>)))
  (:method empty ((<i> <set*-from-collection>)) (empty (base-interface <i>)))
  (:method empty-p ((<i> <set*-from-collection>) x) (empty-p (base-interface <i>) x)))

(define-interface <set-from-map> (<set> <set*-from-collection>) ()
  (:parametric (base-interface)
    (make-interface :base-interface base-interface))
  (:method member-p ((<i> <set-from-map>) element set)
    (nth-value 1 (lookup (base-interface <i>) set element)))
  (:method for-each ((<i> <set-from-map>) set f)
    (for-each* (base-interface <i>) set #'(lambda (k v) v (funcall f k))))
  (:method fold-left ((<i> <set-from-map>) set f seed)
    (fold-left* (base-interface <i>) set #'(lambda (a k v) v (funcall f a k)) seed))
  (:method fold-right ((<i> <set-from-map>) set f seed)
    (fold-right* (base-interface <i>) set #'(lambda (k v a) v (funcall f k a)) seed))
  (:method set-list ((<i> <set-from-map>) set)
    (mapcar 'car (map-alist (base-interface <i>) set)))
  (:method list-set ((<i> <set-from-map>) list)
    (alist-map (base-interface <i>) (mapcar #'cons--t list)))
  (:method size ((<i> <set-from-map>) set)
    (size (base-interface <i>) set)))

(define-interface <multiset-from-map> (<multiset> <set*-from-collection>)
  ((base-interface :type <map> :initarg :base-interface :reader base-interface))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method member-count ((<i> <multiset-from-map>) element multiset)
     (or (lookup (base-interface <i>) multiset element) 0))
  (:method for-each* ((<i> <multiset-from-map>) multiset f)
     (for-each* (base-interface <i>) multiset f))
  (:method fold-left* ((<i> <multiset-from-map>) multiset f seed)
     (fold-left* (base-interface <i>) multiset f seed))
  (:method fold-right* ((<i> <multiset-from-map>) multiset f seed)
     (fold-right* (base-interface <i>) multiset f seed))
  (:method multiset-list ((<i> <multiset-from-map>) multiset)
     (fold-right*
      (base-interface <i>) multiset
      #'(lambda (k v m)
	  (append (make-list v :initial-element k) m))
      nil))
  (:method size ((<i> <multiset-from-map>) multiset)
    (monoid-fold (base-interface <i>) <integer> multiset (constantly 1))))

(define-interface <set-from-multiset> (<set-from-map>) ()
  (:parametric (base-interface)
    (make-interface :base-interface base-interface))
  (:method member-p ((<i> <set-from-multiset>) element set)
    (member-p (base-interface <i>) element set)))

(define-interface <multiset-from-set> (<multiset> <set*-from-collection>)
  ((base-interface :type <map> :initarg :base-interface :reader base-interface))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface))
  (:method member-count ((<i> <multiset-from-set>) element multiset)
     (boolean-integer (member-p (base-interface <i>) element multiset)))
  (:method for-each* ((<i> <multiset-from-set>) multiset f)
     (for-each (base-interface <i>) multiset (compose f #'cons--1)))
  (:method fold-left* ((<i> <multiset-from-set>) multiset f seed)
     (fold-left (base-interface <i>) multiset #'(lambda (a x) (funcall f a x 1)) seed))
  (:method fold-right* ((<i> <multiset-from-set>) multiset f seed)
     (fold-right (base-interface <i>) multiset #'(lambda (x a) (funcall f x 1 a)) seed))
  (:method multiset-list ((<i> <multiset-from-set>) multiset)
     (set-list (base-interface <i>) multiset))
  (:method size ((<i> <multiset-from-set>) multiset)
    (size (base-interface <i>) multiset)))
