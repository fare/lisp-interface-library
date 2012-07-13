;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Order

#+xcvb
(module
 (:depends-on
  ("pure/iterator-interface")))

(in-package :pure)

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

(define-interface <decreasing-number-iterator> (<iterator>)
  ((start :initarg :start :reader iterator-start)
   (end :initarg :end :initform 0 :reader iterator-end)
   (increment :initarg :increment :initform 1 :reader iterator-increment)))

(defmethod next ((i <decreasing-number-iterator>) n)
  (if (and n (< (iterator-end i) n))
      (values t (- n (iterator-increment i)) n)
      (values nil nil nil)))

(define-interface <increasing-number-iterator> (<iterator>)
  ((start :initarg :start :initform 0 :reader iterator-start)
   (end :initarg :end :reader iterator-end)
   (increment :initarg :increment :initform 1 :reader iterator-increment)))

(defmethod next ((i <increasing-number-iterator>) n)
  (if (and n (< n (iterator-end i)))
      (values t (+ n (iterator-increment i)) n)
      (values nil nil nil)))

(defun boolean-integer (bool)
  (if bool 1 0))

(defun make-number-iterator (&rest keys
                             &key from downfrom upfrom
                             to downto upto below above
                             by repeat)
  "Make a number iterator in the style of LOOP"
  (unless (and (typep repeat '(or null integer))
               (loop :for x :in (list from downfrom upfrom to downto upto below above by repeat)
                 :always (typep x '(or null real))))
    (error "Invalid number provided to ~S: ~S" 'make-number-iterator keys))
  (let* ((increment
          (cond
            (by)
            (t 1)))
         (up (or upfrom upto below
                 (and (or from to)
                      (not (or upfrom upto below repeat
                               downfrom downto above)))))
         (down (or downfrom downto above))
         (start
          (cond
            ((or from downfrom upfrom repeat))
            (up 0)))
         (end
          (cond
            ((or below above))
            (repeat 0)
            ((or (and up to) upto)
             (+ (or (and up to) upto) increment))
            ((or (and down to) downto)
             (- (or (and down to) downto) increment)))))
    (unless (and (= 1 (+ (boolean-integer up) (boolean-integer down) (boolean-integer repeat)))
                 (not (and down (not start)))
                 (not (and repeat (or upfrom upto below by downfrom downto above from to)))
                 (> 2
                    (+ (boolean-integer from) (boolean-integer downfrom)
                       (boolean-integer upfrom) (boolean-integer repeat)))
                 (> 2
                    (+ (boolean-integer to) (boolean-integer upto)
                       (boolean-integer downto) (boolean-integer repeat)
                       (boolean-integer above) (boolean-integer below))))
      (error "Invalid combination of keywords for ~S: ~S"
             'make-number-iterator keys))
    (unless (plusp increment)
      (error "Invalid non-positive increment ~S for ~S" increment 'make-number-iterator))
    (cond
      (up
       (make-instance '<increasing-number-iterator>
                      :start start :end end :increment increment))
      ((or down repeat)
       (make-instance '<decreasing-number-iterator>
                      :start start :end end :increment increment))
      (t
       (error "WTF? ~S" 'make-number-iterator)))))

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
