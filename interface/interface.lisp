;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module (:depends-on ("interface/package")))

;; TODO: split into more files.

(in-package :interface)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass interface-class (standard-class)
    ((generics :initform (make-hash-table :test 'eql) :accessor interface-generics)))

  (defmethod closer-mop:validate-superclass
      ((class interface-class) (super-class standard-class))
    t)

  (defun register-interface-generic
      (class name &rest keys &key lambda-list in out)
    (declare (ignore lambda-list in out))
    (setf (gethash name (interface-generics (find-class class))) keys)
    (values)))

  (defun interface-direct-generics (interface)
    (loop :for name :being :the :hash-key :of (interface-generics interface)
      :collect name))

  (defgeneric interface-all-generics (interface)
    (:method ((symbol symbol))
      (interface-all-generics (find-class symbol)))
    (:method ((class interface-class))
      (remove-duplicates
       (loop :for class :in (closer-mop:class-precedence-list class)
         :when (typep class 'interface-class)
         :append (interface-direct-generics class)))))

(defmacro define-interface (interface super-interfaces slots &rest options)
  (let ((class-options
         (remove '(:default-initargs :documentation :metaclass)
                 options :key 'car :test-not #'(lambda (x y) (member y x))))
        (metaclass (find :metaclass options :key 'car))
        (generics (remove :generic options :key 'car :test-not 'eq))
        (methods (remove :method options :key 'car :test-not 'eq))
        (singleton (find :singleton options :key 'car))
        (parametric (find :parametric options :key 'car)))
    `(progn
       (defclass ,interface ,super-interfaces ,slots
         ,@(unless metaclass `((:metaclass interface-class)))
         ,@class-options)
       ,@(when (or parametric singleton)
           (destructuring-bind (formals &body body)
               (or (cdr parametric)
                   '(() (make-interface)))
             `((define-memo-function
                   (,interface
                    :normalization
                    #'(lambda (make-interface &rest arguments)
                        (flet ((make-interface (&rest arguments)
                                 (apply make-interface arguments)))
                          (apply #'(lambda ,formals
                                     (block ,interface
                                       ,@body))
                                 arguments))))
                   (&rest arguments)
                 (apply 'make-instance ',interface arguments)))))
       ,@(when singleton `((defvar ,interface (,interface))))
       ,@(loop :for (nil . generic) :in generics :append
           (destructuring-bind (name &optional interface-options lambda-list
                                     &rest generic-options)
               generic
             `(,@(when lambda-list `((defgeneric ,name ,lambda-list ,@generic-options)))
                 (apply 'register-interface-generic
                        ',interface ',name :lambda-list ',lambda-list
                        ',interface-options))))
       ,@(loop :for (nil . method) :in methods :collect
           `(defmethod ,@method))
       ',interface)))

(define-interface <interface> ()
  ()
  (:documentation "An interface, encapsulating an algorithm"))

(define-interface <type> (<interface>) ()
  (:documentation "An interface encapsulating a particular type of objects")
  (:generic
   make () (<type> &key #+sbcl &allow-other-keys)
   ;; the #+sbcl works around SBCL bug https://bugs.launchpad.net/sbcl/+bug/537711
   (:documentation "Given a <type>, create an object conforming to the interface
based on provided initarg keywords, returning the object."))
  (:generic
   check-invariant () (<type> object &key #+sbcl &allow-other-keys)
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
   empty () (<emptyable>)
   (:documentation "Return an empty object of the emptyable type.
A constant one is pure, a new one if stateful."))
  (:generic
   empty-p () (<emptyable> object)
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
