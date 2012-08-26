;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

;;;; Interface

;;; A class for box objects themselves
(defclass box () ())

(defgeneric box-ref (box)
  (:documentation "open a box and return its contents"))

;;; An interface for boxes

;;; A box: you can make it, or get something out of it
(define-interface <box> (<interface>)
  ()
  (:generic
   make-box (<box> generator &key #+sbcl &allow-other-keys)
   (:out 0) (:values box)
   (:documentation "Make a box from a generator for the value inside the box"))
  (:generic
   unbox (<box> box)
   (:in 1) (:values value)
   (:documentation "Return the value inside the box")))


;;; Classy box: same, based on a class
(define-interface <classy-box> (<box> <classy>) ())

(defmethod make-box ((i <classy-box>) generator &rest keys &key #+sbcl &allow-other-keys)
  (apply 'make i :generator generator keys))

(defmethod unbox ((i <classy-box>) box)
  (declare (ignorable i))
  (box-ref box))


;;;; Boxes that hold a value

(defclass value-box (box)
  ((value :initarg :value :reader box-value)))

(defmethod box-ref ((box value-box))
  (if (slot-boundp box 'value)
    (box-value box)
    (call-next-method)))

;;(defgeneric peek (<box> box reader))
;;(defmethod peek ((i value-box) box reader) (funcall reader (box-value box)))

(defclass simple-value-box (value-box)
  ((value :initarg :generator)))

(defmethod box-ref ((box simple-value-box))
  (box-value box))

(define-interface <value-box> (<classy-box>)
  ((class :initform 'simple-value-box))
  (:singleton))

;;;; Boxes that hold a computation

(defclass thunk-box (box)
  ((thunk :initarg :thunk :reader box-thunk)))

(defclass simple-thunk-box (box)
  ((thunk :initarg :generator)))

(defmethod box-ref ((box simple-thunk-box))
  (funcall (box-thunk box)))

(define-interface <thunk-box> (<classy-box>)
  ((class :initform 'simple-thunk-box)))


;;;; Boxes that hold a promise

(defclass promise-box (value-box simple-thunk-box immutable-box) ())

(define-interface <promise-box> (<value-box> <thunk-box>)
  ((class :initform 'promise-box)))

(defmacro delay (&body body)
  `(make-instance 'promise-box :thunk #'(lambda () ,@body)))

(defun force (promise)
  (box-ref promise))


;;;; Boxes that can only be used once
(defclass one-use-box (box)
  ((usedp :type boolean :initform nil :accessor box-usedp)))

(defun one-use-box (x)
  (make-instance 'one-use-box :value x))

(define-interface <one-use-box> (<classy-box>)
  ((class :initform 'one-use-box)))

(defmethod box-ref :before ((box one-use-box))
  (when (box-usedp box)
    (error "Tried to use ~A more than once" box)))

(defmethod box-ref :after ((box one-use-box))
  (setf (box-usedp box) t))

;;; Some concrete classes following that pattern.
(defclass one-use-value-box (one-use-box value-box) ())
(define-interface <one-use-value-box> (<one-use-box> <value-box>)
  ((class :initform 'one-use-value-box))
  (:singleton))

(defclass one-use-thunk-box (one-use-box thunk-box) ())
(define-interface <one-use-thunk-box> (<one-use-box> <thunk-box>)
  ((class :initform 'one-use-thunk-box)))

(defun make-one-use-function (function &optional name)
  (let ((usedp nil))
    (lambda (&rest args)
      (cond
        ((not usedp)
         (let ((fun function))
           (setf usedp t function nil)
           (apply fun args)))
        (t
         (error "Function ~@[~A ~]already called once" name))))))

(defmacro one-use-lambda (formals &body body)
  `(make-one-use-function #'(lambda ,formals ,@body)))


;;; Some boxes can be empty
(define-interface <emptyable-box> (<box>) ())
(defclass emptyable-box (box) ())

;;; Some boxes can be refilled

(defclass mutable-box (box) ())
(defclass immutable-box (box) ())

(define-interface <mutable-box> (<box>) ())

(defgeneric box-set! (box value)
  (:documentation "set the contents of a BOX (if applicable). Return VALUE."))

(defmethod box-set! ((box immutable-box) value)
  (declare (ignorable box value))
  (error "Trying to set an immutable box"))

(defgeneric (setf box-ref) (value box))

(defmethod (setf box-ref) (value box)
  (box-set! box value))

(defgeneric set-box! (<box> box value))

(defmethod set-box! ((i <classy-box>) box value)
  (declare (ignorable i))
  (box-set! box value))

(defclass box! (mutable-box emptyable-box value-box)
  ((value :writer set-box-value)))

(defun box! (x)
  (make-instance 'box! :value x))

(define-interface <box!> (<mutable-box> <classy-box> <emptyable-box>)
  ((class :initform 'box!)))

(defmethod box-set! ((box box!) value)
  (set-box-value value box))

(defmethod empty-p ((i <box!>) box)
  (declare (ignorable i))
  (slot-boundp box 'value))

(defmethod empty ((i <box!>))
  (declare (ignorable i))
  (make-instance 'box!))
