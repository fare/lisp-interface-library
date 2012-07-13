;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module ())

;;; On this "Interface-Passing Style" of programming, see
;;;  http://fare.livejournal.com/155094.html

(in-package :cl)

(defpackage :interface
  (:use :cl :fare-memoization)
  (:export

   ;;; Classes
   #:<interface>
   #:<type>
   #:<classy>

   ;;; Macros
   #:define-interface
   #:make-interface

   ;;; General purpose gfs
   #:check-invariant
   #:make
   #:update
   #:base-interface
   #:instantiate
   #:convert

   ;;; Boxes!
   #:box #:box-ref #:box-set!
   #:<box> #:make-box #:unbox
   #:<classy-box>
   #:<value-box> #:value-box #:simple-value-box
   #:<thunk-box> #:thunk-box #:simple-thunk-box
   #:<promise-box> #:promise-box #:delay #:force
   #:<one-use-box> #:one-use-box
   #:<one-use-value-box> #:one-use-value-box
   #:<one-use-thunk-box> #:one-use-thunk-box
   #:make-one-use-function #:one-use-lambda
   #:<emptyable-box> #:empty #:empty-p
   #:<mutable-box> #:mutable-box #:immutable-box #:set-box!
   #:<box!> #:box!
   ))

(in-package :interface)

(defmacro define-interface (name super-interfaces slots &rest options)
  (let ((class-options
         (remove-if #'(lambda (x) (member x '(:singleton :parametric))) options :key 'car))
        (singleton (find :singleton options :key 'car))
        (parametric (find :parametric options :key 'car)))
    `(progn
       (defclass ,name ,super-interfaces ,slots ,@class-options)
       ,@(when (or parametric singleton)
           (destructuring-bind (formals &body body)
               (or (cdr parametric)
                   '(() (make-interface)))
             `((define-memo-function
                   (,name :normalization
                          #'(lambda (make-interface &rest arguments)
                              (flet ((make-interface (&rest arguments)
                                       (apply make-interface arguments)))
                                (apply #'(lambda ,formals
                                           (block ,name
                                             ,@body))
                                       arguments))))
                   (&rest arguments)
                 (apply 'make-instance ',name arguments)))))
       ,@(when singleton `((defvar ,name (,name))))
       ',name)))

(define-interface <interface> ()
  ()
  (:documentation "An interface, encapsulating an algorithm"))

(define-interface <type> (<interface>) ()
  (:documentation "An interface encapsulating a particular type of objects"))

(defgeneric make (<type> &key)
  (:documentation "Given a <type>, create an object conforming to the interface
based on provided initarg keywords, returning the object."))

(defgeneric update (<type> object &key)
  (:documentation "Update OBJECT by overriding some of its slots
with those specified as initarg keywords, returning a new object."))

(defgeneric check-invariant (<type> object &key) ;; &allow-other-keys ???
  (:documentation "Check whether an OBJECT fulfills the invariant(s) required
to play a given ROLE with respect to the given INTERFACE.
Interface is an interface, role is a class or keyword,
object is whatever makes sense.
On success the OBJECT itself is returned. On failure an error is signalled."))

(defmethod check-invariant :around (type object &key #+sbcl &allow-other-keys)
  ;; the #+sbcl works around SBCL bug https://bugs.launchpad.net/sbcl/+bug/537711
  (declare (ignorable type))
  (call-next-method)
  object)

(defgeneric base-interface (<interface>)
  (:documentation "from the parametric variant of a mixin, extract the base interface"))


;;; Classy Interface (i.e. has some associated class)

(define-interface <classy> (<interface>)
  ((class :reader interface-class :allocation :class)))

(defgeneric instantiate (<interface> &key &allow-other-keys))

(defmethod instantiate ((i <classy>) &rest keys &key &allow-other-keys)
  (apply 'make-instance (interface-class i) keys))


;;; Conversion between interfaces.

(defgeneric convert (<interface>2 <interface>1 object)
  (:documentation "Convert an OBJECT from <INTERFACE>1 to <INTERFACE>2."))
