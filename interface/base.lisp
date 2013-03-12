;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic Interfaces

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

(define-interface <type> (<interface>) ()
  (:documentation "An interface encapsulating a particular type of objects")
  (:abstract)
  (:generic> check-invariant (object &key #+sbcl &allow-other-keys)
   (:in 1) (:values object) (:out nil) ;; :out nil because normal methods don't return anything!
   (:documentation "Check whether an OBJECT fulfills the invariant(s) required
to be an object of the type represented by this interface.
On success the OBJECT itself is returned. On failure an error is signalled.")
   (:method> :around (object &key #+sbcl &allow-other-keys)
      (call-next-method)
      object))
  (:generic convert (<destination> <origin> object)
   (:values object) (:out 0)
   (:documentation "Convert an OBJECT following interface <ORIGIN>
    into a new object following interface <DESTINATION>."))
  (:generic> create (contents &key #+sbcl &allow-other-keys)
   (:values object) (:out 0)
   (:documentation "create an object conforming to the interface
based on CONTENTS and provided keyword options, returning the object."))
  (:generic> contents (object &key #+sbcl &allow-other-keys)
   (:values contents) (:in 1)
   (:documentation "Given an object, return contents sufficient to re-CREATE a similar object.")))


(define-interface <any> (<type>) ()
  (:singleton)
  (:documentation "Interface for any object")
  (:method> check-invariant (object &key) (declare (ignore object)) (values))
  (:method> convert (<origin> object) (declare (ignore <origin>)) object))

(define-interface <magma> (<type>) ()
  (:abstract)
  (:generic> op (x y))) ; x * y
(define-interface <semigroup> (<magma>) () ;; associativity: (== (op (op x y) z) (op x (op y z)))
  (:abstract)
  (:generic> op/list (list))) ; if not a monoid, the list must be non-empty
(define-interface <identity> (<magma>) () ;; identity: (== x (op id x) (op x id))
  (:abstract)
  (:generic> id ()))
(define-interface <monoid> (<semigroup> <identity>) () ;; associativity, identity
  (:abstract))

#|
(define-interface <quasigroup> (<magma>) () ;; division
  (:abstract)
  (:generic> left-division (x y)) ; x \ y  (== (op x (left-division x y)) y)
  (:generic> right-division (x y))) ; x / y  (== x (op (right-division x y) y))
(define-interface <loop> (<quasigroup> <identity>) () ;; division, identity
|#
(define-interface <group> (<monoid> #|<quasigroup>|#) () ;; associativity, identity, division
  (:generic> inverse (x)))
(define-interface <semiring> (<group>) ()
  (:generic> multiplicative-operation ()))


(define-interface <copyable> (<type>) ()
  (:documentation "A type of objects that can be copied")
  (:abstract)
  (:generic> copy (object)
   (:in 1) (:values object) (:out 0)
   (:documentation "Copy an OBJECT, returning a fresh object with
equivalent contains.
Beware: how deep the copy goes depends on the interface;
copying a data structure may lead to identical objects rather than copies
being left at leaves; or it may not.
If you work in a stateful style, then this matters a lot
because your side-effects may or may not be seen by more or fewer copies
than you think. Please consult the documentation of appropriate methods.")))

(define-interface <foldable> (<type>) ()
  (:documentation "A type of objects that can be folded")
  (:abstract)
  (:generic> monoid-fold (<monoid> foldable function) (:in 2) (:values value)
   (:documentation "Fold the FOLDABLE according to a <MONOID> interface,
where each entry is mapped to the monoid using the provided function"))
  (:generic> monoid-fold* (<monoid> foldable function) (:in 2) (:values value)
   (:documentation "Fold the FOLDABLE according to a <MONOID> interface,
where each entry's values are mapped to the monoid using the provided function"))
  (:generic> fold-left (foldable function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional left to a notional right.
The function takes as parameters
an accumulated value followed by
a single value for the content of each iteration
and returns a new accumulated value."))
  (:generic> fold-left* (foldable function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional left to a notional right.
The function takes as parameters
an accumulated value followed by
as many values as make sense for the specific interface, i.e.
element for a set, element and count for a multi-set, key and value for a map,
and returns a new accumulated value."))
  (:generic> fold-right (foldable function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional right to a notional left.
The function takes as parameters
a single value for the content of each iteration
followed by an accumulated value
and returns a new accumulated value."))
  (:generic> fold-right* (map function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional right to a notional left.
The function takes as parameters
as many values as make sense for the specific interface,
i.e. element for a set, element and count for a multi-set, key and value for a map,
followed by an accumulated value
and returns a new accumulated value."))
  (:generic> for-each (iterator f) (:in 1) (:values result)
   (:documentation "For every step in iterator, apply f to one entry"))
  (:generic> for-each* (iterator f) (:in 1) (:values result)
   (:documentation "For every step in iterator, apply f to multiple values")))

(define-interface <copy-is-identity> (<copyable>) ()
  (:abstract)
  (:method> copy (x)
    x)
  (:documentation "Pure Persistent Data Structures have trivial copying"))

;;; This one is only colloquial for use in pure datastructure. TODO: Move it to pure-?
(defgeneric update (<type> object &key)
  (:documentation "Update OBJECT by overriding some of its slots
with those specified as initarg keywords, returning a new object."))

(define-interface <has-base-interface> (<interface>) ()
  (:generic> base-interface () (:values base-interface)
    (:documentation "from a functor, extract the base interface parameter")))


;;; Makeable
(define-interface <makeable> (<type>) ()
  (:generic> make (&key #+sbcl &allow-other-keys)
   (:values object) (:out 0)
   ;; the #+sbcl works around SBCL bug https://bugs.launchpad.net/sbcl/+bug/537711
   (:documentation "create an object conforming to the interface
based on provided initarg keywords, returning the object.")))


;;; Classy Interface (i.e. has some associated class)

(define-interface <classy> (<makeable>)
  ((class :reader interface-class :allocation :class))
  (:method> make (&rest keys &key #+sbcl &allow-other-keys)
     (apply 'make-instance (interface-class <classy>) keys)))


;;; Size
(define-interface <sizable> (<type>) ()
  (:abstract)
  (:generic> size (object) (:in 1) (:values size) (:out nil)
   (:documentation "Size the object, e.g. number of elements in a collection"))
  (:generic> size<=n-p (object n) (:in 1) (:values boolean) (:out nil)
   (:documentation "Is the size of the object less or equal to integer n?")))

;;; Emptyable
(define-interface <emptyable> (<type>) ()
  (:abstract)
  (:generic> empty ()
   (:values object) (:out 0)
   (:documentation "Return an empty object of the emptyable type.
A constant one is pure, a new one if stateful."))
  (:generic> empty-p (object)
   (:in 1) (:values boolean)
   (:documentation "Return a boolean indicating whether the object is empty")))

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

;;;
;;; Simple Mixins
;;;
(defclass empty-object () ())
(defun make-empty-object ()
  (make-instance 'empty-object))
(defun empty-object-p (x)
  (typep x 'empty-object))

(define-interface <empty-is-empty-object> (<emptyable>) ()
  (:abstract)
  (:method> check-invariant ((m empty-object) &key &allow-other-keys)
    (values))
  (:method> empty ()
    (make-empty-object))
  (:method> empty-p (object)
    (empty-object-p object)))

(define-interface <empty-is-nil> (<emptyable>) ()
  (:abstract)
  (:method> check-invariant ((m null) &key &allow-other-keys)
    (values))
  (:method> empty ()
    nil)
  (:method> empty-p (object)
    (null object)))

(define-interface <foldable-*-from> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold* (<monoid> map function)
    (monoid-fold <monoid> map function))
  (:method> fold-left* (map function seed)
    (fold-left map function seed))
  (:method> fold-right* (map function seed)
    (fold-right map function seed))
  (:method> for-each* (map function)
    (for-each map function)))

(define-interface <foldable-monoid-fold-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold (<monoid> map fun)
    (fold-left
     map
     #'(lambda (acc entry) (op <monoid> acc (funcall fun entry)))
     (id <monoid>))))

(define-interface <foldable-fold-right-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> fold-right (map right-function seed)
    (funcall
     (fold-left
      map
      #'(lambda (f entry) #'(lambda (accumulator) (funcall f (funcall right-function entry accumulator))))
      #'identity)
     seed)))

(define-interface <foldable-for-each-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> for-each (map fun)
    (fold-left
     map
     #'(lambda (s e) (declare (ignore s)) (funcall fun e))
     nil)
    (values)))

(define-interface <foldable-size-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> size (map)
    (fold-left map #'(lambda (x e) (declare (ignore e)) (1+ x)) 0)))

(define-interface <sizable-size<=n-p-from-size> (<sizable>) ()
  (:abstract)
  (:method> size<=n-p (map n)
    (<= (size map) n)))

;;; TODO: move this somewhere else!
(defun boolean-integer (bool)
  (if bool 1 0))
