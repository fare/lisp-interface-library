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
    into a new object following interface <DESTINATION>.")))

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
  (:generic> fold-left (foldable f seed) (:in 1) (:values value)
   (:documentation "Fold an object with a function,
iterating on the contents from a notional left to a notional right.
The function takes as parameters
an accumulated value followed by
a single value for the content of each iteration
and returns a new accumulated value."))
  (:generic> fold-left* (map f seed) (:in 1) (:values value)
   (:documentation "Fold an object with a function,
iterating on the contents from a notional left to a notional right.
The function takes as parameters
an accumulated value followed by
as many values as make sense for the specific interface, i.e.
element for a set, element and count for a multi-set, key and value for a map,
and returns a new accumulated value."))
  (:generic> fold-right (foldable f seed) (:in 1) (:values value)
   (:documentation "Fold an object with a function,
iterating on the contents from a notional right to a notional left.
The function takes as parameters
a single value for the content of each iteration
followed by an accumulated value
and returns a new accumulated value."))
  (:generic> fold-right* (map f seed) (:in 1) (:values value)
   (:documentation "Fold an object with a function,
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

(defgeneric base-interface (<interface>)
  (:documentation "from a functor, extract the base interface parameter"))

(defgeneric key-interface (<map>)
  (:documentation "Interface for the type of keys of a map"))

(defgeneric value-interface (<map>)
  (:documentation "Interface for the type of values of a map"))


;;; Makeable
(define-interface <makeable> (<type>) ()
  (:generic> make (&key #+sbcl &allow-other-keys)
   (:values object) (:out 0)
   ;; the #+sbcl works around SBCL bug https://bugs.launchpad.net/sbcl/+bug/537711
   (:documentation "Given a <type>, create an object conforming to the interface
based on provided initarg keywords, returning the object.")))

;;; Classy Interface (i.e. has some associated class)

(define-interface <classy> (<makeable>)
  ((class :reader interface-class :allocation :class)))

(defmethod make ((i <classy>) &rest keys &key #+sbcl &allow-other-keys)
  (apply 'make-instance (interface-class i) keys))

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
  (:generic> first-entry (collection) (:in 1) (:values entry foundp)
   (:documentation "Return two values:
1- a single value for an entry
2- a boolean a boolean indicating whether the collection was already empty.
What 'first' means here may depend on the particular collection interface,
but generally means the element most easily accessible;
it is also the first (leftmost) key and value as used by fold-left and fold-right."))
  (:generic> entry-values (entry)
   (:documentation "Take one entry value, return as many values as makes sense for the entry.")))


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
