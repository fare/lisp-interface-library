;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("interface/base")))

(in-package :interface)

;;;; Interface

;;; A class for box objects themselves
(defclass box () ())

(defgeneric box-ref (box)
  (:documentation "open a box and return its contents"))

;;; An interface for boxes

;;; A box: you can make it, or get something out of it
(define-interface <box> (<interface>) ()
  (:abstract)
  (:generic
   make-box (<box> generator &key #+sbcl &allow-other-keys)
   (:out 0) (:values box)
   (:documentation "Make a box from a generator for the value inside the box"))
  (:generic
   unbox (<box> box)
   (:in 1) (:values value)
   (:documentation "Return the value inside the box")))


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

(define-interface <value-box> (<box>)
  ((class :initform 'simple-value-box))
  (:singleton))

;;;; Boxes that hold a computation

(defclass thunk-box (box)
  ((thunk :initarg :thunk :reader box-thunk)))

(defclass simple-thunk-box (box)
  ((thunk :initarg :generator)))

(defmethod box-ref ((box simple-thunk-box))
  (funcall (box-thunk box)))

(define-interface <thunk-box> (<box>)
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

(define-interface <one-use-box> (<box>)
  ((class :initform 'one-use-box)))

(defmethod box-ref :before ((box one-use-box))
  (when (box-usedp box)
    (error "Tried to use ~A more than once" box)))

(defmethod box-ref :after ((box one-use-box))
  (setf (box-usedp box) t))

;;; Some concrete classes following that pattern.
(defclass one-use-value-box (one-use-box simple-value-box) ())
(define-interface <one-use-value-box> (<one-use-box> <value-box>)
  ((class :initform 'one-use-value-box))
  (:singleton))

(defclass one-use-thunk-box (one-use-box thunk-box) ())
(define-interface <one-use-thunk-box> (<one-use-box> <thunk-box>)
  ((class :initform 'one-use-thunk-box)))

(defmethod box-ref :after ((box one-use-value-box))
  (setf (slot-value box 'value) nil)) ;; also clear the used up value.

(defmethod box-ref :after ((box one-use-thunk-box))
  (setf (slot-value box 'thunk) nil)) ;; also clear the used up thunk.

(defun one-use-value-box (x)
  (make-instance 'one-use-value-box :value x))

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
(define-interface <emptyable-box> (<emptyable> <box>) ()
  (:abstract))
(defclass emptyable-box (box) ())

;;; Some boxes can be refilled

(defclass mutable-box (box) ())
(defclass immutable-box (box) ())

(define-interface <mutable-box> (<box>) ()
  (:abstract))

(defgeneric box-set! (box value)
  (:documentation "set the contents of a BOX (if applicable). Return VALUE."))

(defmethod box-set! ((box immutable-box) value)
  (declare (ignorable box value))
  (error "Trying to set an immutable box"))

(defgeneric (setf box-ref) (value box))

(defmethod (setf box-ref) (value box)
  (box-set! box value))

(defgeneric set-box! (<box> box value))

(defmethod set-box! ((i <box>) box value)
  (declare (ignorable i))
  (box-set! box value))

(defclass box! (mutable-box emptyable-box value-box)
  ((value :writer set-box-value :accessor box-value)))

(defun box! (x)
  (make-instance 'box! :value x))

(define-interface <box!> (<mutable-box> <emptyable-box>)
  ((class :initform 'box!)))

(defmethod box-set! ((box box!) value)
  (setf (box-value box) value))

(defmethod empty-p ((i <box!>) box)
  (declare (ignorable i))
  (slot-boundp box 'value))

(defmethod empty ((i <box!>))
  (declare (ignorable i))
  (make-instance 'box!))
