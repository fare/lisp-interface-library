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
  (:documentation "from the parametric variant of a mixin, extract the base interface"))

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
   (:documentation "Size the object, e.g. number of elements in a map"))
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

(define-interface <empty-is-nil> (<emptyable>) ()
  (:abstract)
  (:method> check-invariant ((m null) &key &allow-other-keys)
    (values))
  (:method> empty ()
    nil)
  (:method> empty-p (object)
    (null object)))

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

;;; Iteration
(defgeneric for-each (<interface> iterator f)
  (:documentation "For every step in iterator, apply f to values"))

;;; TODO: move this somewhere else!
(defun boolean-integer (bool)
  (if bool 1 0))

