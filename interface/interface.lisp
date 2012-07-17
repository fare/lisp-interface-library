;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb (module ())

;; TODO: split into more files.

(in-package :cl)

(defpackage :interface
  (:use :closer-common-lisp :fare-memoization :closer-mop)
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
   #:size #:size<=n-p
   #:for-each

   ;;; Empty?
   #:<emptyable> #:empty #:empty-p

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

   ;;; Maps, trees, etc.
   ;; Do NOT export these interfaces, because pure and stateful branch them.
   ;; #:<map> #:<alist> #:<tree> #:<binary-tree> #:<avl-tree> #:<number-map> #:<nm> #:<fmim>
   ;; DO export the accessors, because pure and stateful use them.
   #:lookup #:first-key-value #:fold-left #:fold-right #:map-alist #:alist-map
   #:node #:node-key #:node-value #:left #:right #:node-height #:node-balance
   #:locate #:leftmost #:rightmost

   ;; number iterators
   #:make-number-iterator
   #:<number-iterator> #:<decreasing-number-iterator> #:<increasing-number-iterator>
   ;; TODO: move this somewhere else
   #:boolean-integer

   ;; simple mixins
   #:map-cheap-size #:map-for-each-from-fold-left
   #:map-fold-right-from-fold-left #:map-size-from-fold-left
   ))

(in-package :interface)

;;;TODO: use MOP for an interface class,
;;; store list of gf signatures that go with the interface in class object
#|
(defclass interface (class)
  xxx)
|#

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

;;; This one is only colloquial for use in pure datastructure. TODO: Move it to pure-?
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

(defgeneric instantiate (<interface> &key #+sbcl &allow-other-keys))

(defmethod instantiate ((i <classy>) &rest keys &key #+sbcl &allow-other-keys)
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
(define-interface <emptyable> (<type>) ())

(defgeneric empty (<emptyable>)
  (:documentation "Return an empty object of the emptyable type.
A constant one is pure, a new one if stateful."))

(defgeneric empty-p (<emptyable> box)
  (:documentation "Return a boolean indicating whether the object is empty"))

;;; Iteration
(defgeneric for-each (<interface> iterator f)
  (:documentation "For every step in iterator, apply f to values"))

;;; TODO: move this somewhere else!
(defun boolean-integer (bool)
  (if bool 1 0))
