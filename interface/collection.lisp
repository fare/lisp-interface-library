;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Read-only interfaces common to pure and stateful collections

(uiop:define-package :lil/interface/collection
  (:use
   :closer-common-lisp
   :lil/interface/definition
   :lil/interface/base)
  (:use-reexport
   :lil/interface/empty
   :lil/interface/size
   :lil/interface/fold
   :lil/interface/iterator)
  (:export
   #:<finite-collection> ;; note: shadowed in pure, stateful
   #:get-entry #:has-key-p #:first-entry #:entry-values
   #:singleton-p #:singleton #:singleton*
   #:key-interface
   #:<collection-has-key-p-from-get-entry>
   #:<encoded-key-collection> #:<parametric-encoded-key-collection> #:encode-key #:decode-key
   #:key-encoder #:key-decoder))
(in-package :lil/interface/collection)

(define-interface <finite-collection> (<sizable> <foldable> <copyable> <emptyable>) ()
  (:abstract)
  (:generic> get-entry (collection key) (:in 1) (:values entry foundp)
   (:documentation "Return two values:
1- a single value for an entry
2- a boolean a boolean indicating whether the entry was found."))
  (:generic> has-key-p (collection key) (:in 1) (:values foundp)
   (:documentation "Return a boolean indicating whether an entry was found for that key."))
  (:generic> key-interface () (:values interface)
   (:documentation "Interface for the type of keys of a collection"))
  (:generic> singleton-p (collection) (:in 1) (:values boolean)
   (:documentation "Is the collection a singleton?"))
  (:generic> singleton (entry) (:out 0) (:values collection)
   (:documentation "Make a singleton from a single entry"))
  (:generic> first-entry (collection) (:in 1) (:values entry foundp)
   (:documentation "Return two values:
1- a single value for an entry
2- a boolean a boolean indicating whether the collection was already empty.
What 'first' means here may depend on the particular collection interface,
but generally means the element most easily accessible;
it is also the first (leftmost) key and value as used by fold-left and fold-right."))
  (:generic> entry-values (entry)
   (:documentation "Take one entry value, return as many values as makes sense for the entry.")))

(define-interface <collection-has-key-p-from-get-entry> (<finite-collection>) ()
  (:method> has-key-p (collection key)
     (nth-value 1 (get-entry collection key))))

(define-interface <encoded-key-collection> (<finite-collection>) ()
  (:generic> encode-key (plain-key)
     (:documentation "encode user-visible key into internal key"))
  (:generic> decode-key (encoded-key)
     (:documentation "decode user-visible key from internal key")))

(define-interface <parametric-encoded-key-collection> (<encoded-key-collection>)
  ((base-interface :initarg :base-interface :reader base-interface) ;; internal map representation
   (key-encoder :initarg :key-encoder :reader key-encoder) ;; from key-interface to (key-interface base-interface)
   (key-decoder :initarg :key-decoder :reader key-decoder)) ;; from (key-interface base-interface) to key-interface
 (:method encode-key ((i <parametric-encoded-key-collection>) k)
  (funcall (key-encoder i) k))
 (:method decode-key ((i <parametric-encoded-key-collection>) k)
  (funcall (key-decoder i) k)))

