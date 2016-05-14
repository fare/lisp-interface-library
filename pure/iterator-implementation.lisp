;;;;; Pure Iterators (implementation)

(uiop:define-package :lil/pure/iterator-implementation
  (:use :closer-common-lisp :lil/core/definition)
  (:use-reexport :lil/pure/iterator))
(in-package :lil/pure/iterator-implementation)

;;; Trivial stream: devnull
(define-interface <devnull> (<fount> <sink>) ())
(defmethod iterator ((i <devnull>) x)
  (declare (ignorable i x))
  nil)
(defmethod next ((i <devnull>) x)
  (declare (ignorable i x))
  (values nil nil))
(defmethod collector ((i <devnull>) x)
  (declare (ignorable i x))
  nil)
(defmethod collect ((i <devnull>) x &rest values)
  (declare (ignorable i x values))
  nil)
(defmethod result ((i <devnull>) x)
  (declare (ignorable i x))
  (values))


;;; Number iterators
(defmethod iterator ((i <number-iterator>) x)
  (declare (ignorable i))
  (iterator-start x))
(defmethod next ((i <decreasing-number-iterator>) n)
  (if (and n (< (iterator-end i) n))
      (values t (- n (iterator-increment i)) n)
      (values nil nil nil)))
(defmethod next ((i <increasing-number-iterator>) n)
  (if (and n (< n (iterator-end i)))
      (values t (+ n (iterator-increment i)) n)
      (values nil nil nil)))

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

(defmethod for-each* ((<fount> <fount>) fount fun)
  (flow <fount> (<for-each>) fount fun))
