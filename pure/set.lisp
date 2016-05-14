;;;;; Pure Sets and Multisets

(uiop:define-package :lil/pure/set
  (:use :closer-common-lisp
        :lil/core/definition
        :lil/interface/base)
  (:use-reexport
   :lil/interface/set
   :lil/pure/collection
   :lil/pure/map)
  (:shadow #:<set> #:<set*> #:<multiset>
           #:<set*-from-collection>
           #:<set-from-map> #:<multiset-from-map> #:<set-from-multiset> #:<multiset-from-set>)
  (:export
   #:set-union #:set-union/list #:set-intersection #:set-intersection/list
   #:set-disjunction #:subset-p #:proper-subset-p
   #:increase-member-count #:decrease-member-count))
(in-package :lil/pure/set)

(define-interface <set*> (<finite-collection> lil/interface/set:<set*>) ()
  (:abstract)
  (:generic> set-union (set1 set2)
    (:documentation "Return a new (multi)SET, union of SET1 and SET2"))
  (:generic> set-union/list (list)
    (:documentation "Return a new (multi)SET, union of a LIST of sets"))
  (:generic> set-intersection (set1 set2)
    (:documentation "Return a new (multi)SET, intersection of SET1 and SET2"))
  (:generic> set-intersection/list (list)
    (:documentation "Return a new (multi)SET, intersection of a LIST of sets"))
  (:generic> set-disjunction (set1 set2)
    (:documentation "Return a new (multi)SET, SET1 minus SET2"))
  (:generic> subset-p (set1 set2)
    (:documentation "Return a boolean, TRUE iff SET1 is a subset of SET2"))
  (:generic> proper-subset-p (set1 set2)
    (:documentation "Return a boolean, TRUE iff SET1 is a proper subset of SET2")))

(define-interface <set> (lil/interface/set:<set> <set*>) ()
  (:abstract))

(define-interface <multiset> (lil/interface/set:<multiset> <set*>) ()
  (:abstract)
  (:generic> increase-member-count (collection key &optional count))
  (:generic> decrease-member-count (collection key &optional count)))

(define-interface <set*-from-collection>
  (lil/interface/set:<set*-from-collection> <set*>) ())

(define-interface <set-from-map> (<set> <set*-from-collection> lil/interface/set:<set-from-map>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface)))

(define-interface <multiset-from-map> (<multiset> <set*-from-collection> lil/interface/set:<multiset-from-map>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface)))

(define-interface <set-from-multiset> (<set> <set*-from-collection> lil/interface/set:<set-from-multiset>)
  ((base-interface :type <multiset>))
  (:parametric (base-interface)
    (make-interface :base-interface base-interface)))

(define-interface <multiset-from-set> (<multiset> <set*-from-collection> lil/interface/set:<multiset-from-set>)
  ((base-interface :type <map>))
  (:parametric (base-interface)
     (make-interface :base-interface base-interface)))
