;;;;; Order

(uiop:define-package :lil/interface/order
  (:use :closer-common-lisp
   :lil/core :lil/interface/base
   :lil/interface/eq :lil/interface/group)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:<order> #:<order-from-lessp> #:<lessp>
   #:<order-from-compare> #:<compare>
   #:order< #:order<= #:order> #:order>= #:== #:compare
   #:<order-parameter> #:order-interface
   #:<boolean> #:<number> #:<integer> #:<char> #:<string> #:<case-insensitive-string>))

(in-package :lil/interface/order)

(define-interface <order> (<eq>) ()
  (:abstract)
  (:generic> order< (x y) (:in 1 2))
  (:generic> order<= (x y) (:in 1 2))
  (:generic> order> (x y) (:in 1 2))
  (:generic> order>= (x y) (:in 1 2))
  (:generic> compare (x y) (:in 1 2)))

(define-interface <order-from-lessp> (<order>) ()
  (:abstract)
  (:method> order<= (x y)
     (not (order< y x)))
  (:method> order> (x y)
     (order< y x))
  (:method> order>= (x y)
     (not (order< x y)))
  (:method> == (x y)
     (not (or (order< x y) (order< y x))))
  (:method> compare (x y)
     (cond
       ((order< x y) -1)
       ((order> x y) 1)
       (t 0))))

(define-interface <order-from-compare> (<order>) ()
  (:abstract)
  (:method> order< (x y)
     (ecase (compare x y)
       ((-1) t)
       ((0 1) nil)))
  (:method> order<= (x y)
     (ecase (compare x y)
       ((-1 0) t)
       (1 nil)))
  (:method> order> (x y)
     (ecase (compare x y)
       ((-1 0) nil)
       ((1) t)))
  (:method> order>= (x y)
     (ecase (compare x y)
       ((-1) nil)
       ((0 1) t)))
  (:method> == (x y)
     (ecase (compare x y)
       ((-1 1) nil)
       ((0) t))))

(define-interface <compare> (<order-from-compare>)
  ((compare-function :initarg :compare :reader compare-function))
  (:parametric (compare) (make-interface :compare compare))
  (:method> compare (x y)
    (funcall (compare-function <compare>) x y)))

(define-interface <lessp> (<order-from-lessp>)
  ((lessp :initarg :lessp :reader lessp-function))
  (:parametric (lessp) (make-interface :lessp lessp)))

(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for (name suffix) :in names :collect
                    `(defmethod ,name ((i <lessp>) x y)
                       (,(symbolicate :call suffix) (lessp-function i) x y))))))
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

(macrolet ((builtin (name super prefix)
             `(progn
                (define-interface ,name (,@super <order>) () (:singleton))
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
  (builtin <number> (<monoid>) "")
  (builtin <char> () char)
  (builtin <string> () string))

(define-interface-methods <number>
  (:method> op (x y) (+ x y))
  (:method> op/list (list) (reduce #'+ list :initial-value 0))
  (:method> id () 0)
  (:method> inverse (x) (- x)))

(define-interface <integer> (<number>) ()
  (:singleton))

(define-interface <case-insensitive-string> (<order-from-lessp>) ()
  (:singleton)
  (:method> order< (x y)
    (string-lessp x y))
  (:method> order<= (x y)
    (string-not-greaterp x y))
  (:method> order> (x y)
    (string-greaterp x y))
  (:method> order>= (x y)
    (string-not-lessp x y))
  (:method> == (x y)
    (string-equal x y))
  (:method> eq-function ()
    #'string-equal))

(define-interface <order-parameter> (<order>)
  ((order-interface :initarg :order :reader order-interface)))
(macrolet ((delegate (&rest names)
             `(progn
                ,@(loop :for name :in names :collect
                    `(defmethod ,name ((i <order-parameter>) x y)
                       (,name (order-interface i) x y))))))
  (delegate order< order<= order> order>= == compare))

(define-interface <boolean> (<eql> <equal> <order>) () ;; uses EQL for comparison, yet sxhash for hash.
  (:singleton)
  (:method> order< (x y) (and y (not x)))
  (:method> order<= (x y) (or y (not x)))
  (:method> order> (x y) (and x (not y)))
  (:method> order>= (x y) (or x (not y)))
  (:method> compare (x y) (cond ((eq x y) 0) (x -1) (t 1))))

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
