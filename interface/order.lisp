;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Order

#+xcvb
(module
 (:depends-on
  ("package"
   "base/strings"
   "base/symbols"
   "interface/interface"
   "interface/eq")))

(in-package :cl)

(defpackage :order
  (:use :interface :eq :cl :alexandria)
  (:export
   #:<order> #:<number> #:<string> #:<char>
   #:<order-from-lessp> #:<lessp>
   #:<order-from-compare> #:<compare>
   #:<key> #:<order-parameter>
   #:order< #:order<= #:order> #:order>= #:== #:compare
   #:order-interface))

(in-package :order)

(define-interface <order> (<eq>) ())
(defgeneric order< (i x y))
(defgeneric order<= (i x y))
(defgeneric order> (i x y))
(defgeneric order>= (i x y))
(defgeneric compare (i x y))

(define-interface <order-from-lessp> (<order>) ())
(defmethod order<= ((i <order-from-lessp>) x y)
  (not (order< i y x)))
(defmethod order> ((i <order-from-lessp>) x y)
  (order< i y x))
(defmethod order>= ((i <order-from-lessp>) x y)
  (not (order< i x y)))
(defmethod == ((i <order-from-lessp>) x y)
  (not (or (order< i x y) (order< i y x))))
(defmethod compare ((i <order-from-lessp>) x y)
  (cond
    ((order< i x y) -1)
    ((order> i x y) 1)
    (t 0)))

(define-interface <order-from-compare> (<order>) ())
(defmethod order< ((i <order-from-compare>) x y)
  (ecase (compare i x y)
    ((-1) t)
    ((0 1) nil)))
(defmethod order<= ((i <order-from-compare>) x y)
  (ecase (compare i x y)
    ((-1 0) t)
    (1 nil)))
(defmethod order> ((i <order-from-compare>) x y)
  (ecase (compare i x y)
    ((-1 0) nil)
    ((1) t)))
(defmethod order>= ((i <order-from-compare>) x y)
  (ecase (compare i x y)
    ((-1) nil)
    ((0 1) t)))
(defmethod == ((i <order-from-compare>) x y)
  (ecase (compare i x y)
    ((-1 1) nil)
    ((0) t)))

(define-interface <compare> (<order-from-compare>)
  ((compare :initarg :compare :reader compare-function))
  (:parametric (compare) (make-interface :compare compare)))
(defmethod compare ((i <compare>) x y)
  (funcall (compare-function i) x y))

(define-interface <lessp> (<order-from-lessp>)
  ((lessp :initarg :lessp :reader lessp-function))
  (:parametric (lessp) (make-interface :lessp lessp)))

(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for (name suffix) :in names :collect
                    `(defmethod ,name ((i <lessp>) x y)
                       (,(symbolicate :call suffix) (lessp-function i)
                              (funcall (key-function i) x)
                              (funcall (key-function i) y)))))))
  (delegate (order< <) (order<= <=) (order> >) (order>= >=)
            (== =) (compare -compare)))

(defun call< (lessp x y)
  (funcall lessp x y))
(defun call<= (lessp x y)
  (not (funcall lessp y x)))
(defun call> (lessp x y)
  (funcall lessp y x))
(defun call>= (lessp x y)
  (not (funcall lessp x y)))
(defun call= (lessp x y)
  (not (or (funcall lessp x y) (funcall lessp y x))))
(defun call-compare (lessp x y)
  (cond
    ((funcall lessp x y) -1)
    ((funcall lessp y x) 1)
    (t 0)))

(macrolet ((builtin (name prefix)
             `(progn
                (define-interface ,name (<order>) () (:singleton))
                ,@(loop :for n :in '(< <= > >=) :collect
                    `(defmethod ,(symbolicate :order n) ((i ,name) x y)
                       (,(symbolicate prefix n) x y)))
                (defmethod == ((i ,name) x y)
                  (,(symbolicate prefix '=) x y))
                (defmethod compare ((i ,name) x y)
                  (cond
                    ((,(symbolicate prefix '<) x y) -1)
                    ((,(symbolicate prefix '>) x y) 1)
                    (t 0))))))
  ;;(builtin function call)
  (builtin <number> "")
  (builtin <char> char)
  (builtin <string> string))

(define-interface <key> ()
  ((order-key :initarg :key :reader key-function)
   (order-key-interface :initarg :order :reader order-interface))
  (:parametric (&key key order) (make-interface :key key :order order)))
(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for name :in names :collect
                    `(defmethod ,name ((i <key>) x y)
                       (,name (order-interface i)
                              (funcall (key-function i) x)
                              (funcall (key-function i) y)))))))
  (delegate order< order<= order> order>= == compare))

(define-interface <order-parameter> ()
  ((order-interface :initarg :order :reader order-interface)))
(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for name :in names :collect
                    `(defmethod ,name ((i <order-parameter>) x y)
                       (,name (order-interface i) x y))))))
  (delegate order< order<= order> order>= == compare))


;;; simple algorithm using order
(defun sorted-list-differences (list1 list2 &key (order <number>))
  (labels
      ((rec (list1 list2 only1 common only2)
         (cond
           ((and (null list1) (null list2))
            (values (nreverse only1) (nreverse common) (nreverse only2)))
           ((null list1)
            (values (nreverse only1) (nreverse common) (nreconc only2 list2)))
           ((null list2)
            (values (nreconc only1 list1) (nreverse common) (nreverse only2)))
           (t
            (let ((r (compare order (car list1) (car list2))))
              (cond
                ((= r 0)
                 (rec (cdr list1) (cdr list2) only1 (cons (car list1) common) only2))
                ((< r 0)
                 (rec (cdr list1) list2 (cons (car list1) only1) common only2))
                (t ;(> r 0)
                 (rec list1 (cdr list2) only1 common (cons (car list2) only2)))))))))
    (rec list1 list2 nil nil nil)))
