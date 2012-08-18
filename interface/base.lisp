;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Basic Interfaces

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

(define-interface <type> (<interface>) ()
  (:documentation "An interface encapsulating a particular type of objects")
  (:generic
   make (<type> &key #+sbcl &allow-other-keys)
   (:values object) (:out 0)
   ;; the #+sbcl works around SBCL bug https://bugs.launchpad.net/sbcl/+bug/537711
   (:documentation "Given a <type>, create an object conforming to the interface
based on provided initarg keywords, returning the object."))
  (:generic
   check-invariant (<type> object &key #+sbcl &allow-other-keys)
   (:in 1) (:values object) (:out 0)
   (:documentation "Check whether an OBJECT fulfills the invariant(s) required
to be an object of the type represented by this interface.
On success the OBJECT itself is returned. On failure an error is signalled.")
   (:method :around (type object &key #+sbcl &allow-other-keys)
      (declare (ignorable type))
      (call-next-method)
      object)))


;;; This one is only colloquial for use in pure datastructure. TODO: Move it to pure-?
(defgeneric update (<type> object &key)
  (:documentation "Update OBJECT by overriding some of its slots
with those specified as initarg keywords, returning a new object."))

(defgeneric base-interface (<interface>)
  (:documentation "from the parametric variant of a mixin, extract the base interface"))


;;; Classy Interface (i.e. has some associated class)

(define-interface <classy> (<type>)
  ((class :reader interface-class :allocation :class)))

(defmethod make ((i <classy>) &rest keys &key #+sbcl &allow-other-keys)
  (apply 'make-instance (interface-class i) keys))


;;; Conversion between interfaces.
(defgeneric convert (<destination> <origin> object)
  (:documentation "Convert an OBJECT from interface <ORIGIN> to interface <DESTINATION>."))

;;; Size
(defgeneric size (<interface> object)
  (:documentation "Size the object, e.g. number of elements in a map"))

(defgeneric size<=n-p (<interface> object n)
  (:documentation "Is the size of the object less or equal to integer n?"))

;;; Emptyable
(define-interface <emptyable> (<type>)
  ()
  (:generic
   empty (<emptyable>)
   (:values object) (:out 0)
   (:documentation "Return an empty object of the emptyable type.
A constant one is pure, a new one if stateful."))
  (:generic
   empty-p (<emptyable> object)
   (:in 1) (:values boolean)
   (:documentation "Return a boolean indicating whether the object is empty")))

;;; Iteration
(defgeneric for-each (<interface> iterator f)
  (:documentation "For every step in iterator, apply f to values"))

;;; TODO: move this somewhere else!
(defun boolean-integer (bool)
  (if bool 1 0))

(defclass empty-object () ())
(defun make-empty-object ()
  (make-instance 'empty-object))
(defun empty-object-p (x)
  (typep x 'empty-object))
