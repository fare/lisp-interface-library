;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Order

#+xcvb (module (:depends-on ("stateful/iterator-interface")))

(in-package :stateful)

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

(defmethod iterator (<number-iterator> iterator)
  (make-box '<box!> :value (iterator-start iterator)))
(defmethod next ((i <decreasing-number-iterator>) counter-box)
  (with-slots (end increment) i
    (let ((counter (box-ref counter-box)))
      (cond
        ((and counter (< end counter))
         (setf (box-ref counter-box) (- counter increment))
         (values t counter))
        (values nil nil)))))
(defmethod next ((i <increasing-number-iterator>) n)
  (with-slots (end increment) i
    (let ((counter (box-ref counter-box)))
      (cond
        ((and counter (< counter end))
         (setf (box-ref counter-box) (+ counter increment))
         (values t counter))
        (values nil nil)))))

(defmethod flow ((<fount> <fount>) (<sink> <sink>) fount sink)
  (labels ((r (iterator collector)
             (multiple-value-call
                 #'(lambda (hasp next &rest values)
                     (if hasp
                         (r next (apply 'collect <sink> collector values))
                         (result <sink> collector)))
               (next <fount> iterator))))
    (r (iterator <fount> fount) (collector <sink> sink))))

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

(defmethod for-each ((<fount> <fount>) fount fun)
  (flow <fount> (<for-each>) fount fun))
