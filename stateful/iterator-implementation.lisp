;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful Iterator (implementation)

(uiop:define-package :lil/stateful/iterator-implementation
  (:use :closer-common-lisp :lil/interface/definition)
  (:use-reexport :lil/stateful/iterator))
(in-package :lil/stateful/iterator-implementation)

;;; Trivial stream: devnull
(define-interface <devnull> (<fount> <sink>) ())
(defmethod iterator ((i <devnull>) x)
  (declare (ignorable i x))
  nil)
(defmethod next ((i <devnull>) x)
  (declare (ignorable i x))
  (values))
(defmethod collector ((i <devnull>) x)
  (declare (ignorable i x))
  nil)
(defmethod collect ((i <devnull>) x &rest values)
  (declare (ignorable i x values))
  (values))
(defmethod result ((i <devnull>) x)
  (declare (ignorable i x))
  (values))

(defmethod iterator ((i <number-iterator>) iterator)
  (make-box '<box!> (iterator-start iterator)))
(defmethod next ((i <decreasing-number-iterator>) counter-box)
  (let ((counter (box-ref counter-box))
	(end (iterator-end i))
	(increment (iterator-increment i)))
    (cond
      ((and counter (< end counter))
       (setf (box-ref counter-box) (- counter increment))
       (values t counter))
      (t
       (values nil nil)))))
(defmethod next ((i <increasing-number-iterator>) counter-box)
  (let ((counter (box-ref counter-box))
	(end (iterator-end i))
	(increment (iterator-increment i)))
    (cond
      ((and counter (< counter end))
       (setf (box-ref counter-box) (+ counter increment))
       (values t counter))
      (t
       (values nil nil)))))

(defmethod flow ((<fount> <fount>) (<sink> <sink>) fount sink)
  (loop :with iterator = (iterator <fount> fount)
    :with collector = (collector <sink> sink)
    :for values = (handler-case (multiple-value-list (next <fount> iterator))
                    (end-of-iteration () (return (result <sink> collector))))
    :do (apply 'collect <sink> collector values)))

;;; A sink that calls a function for side-effect on each collected value
(defmethod collector ((<for-each> <for-each>) fun)
  (declare (ignorable <for-each>))
  fun)
(defmethod collect ((<for-each> <for-each>) fun &rest values)
  (declare (ignorable <for-each>))
  (apply fun values)
  fun)
(defmethod result ((<for-each> <for-each>) fun)
  (declare (ignorable <for-each> fun))
  nil)

(defmethod for-each* ((<fount> <fount>) fount fun)
  (flow <fount> (<for-each>) fount fun))
