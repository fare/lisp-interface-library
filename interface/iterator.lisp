;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; iterator

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

(define-interface <number-iterator> (<interface>)
  ((start :initarg :start :reader iterator-start)
   (end :initarg :end :reader iterator-end)
   (increment :initarg :increment :reader iterator-increment))
  (:abstract))  

(define-interface <decreasing-number-iterator> (<number-iterator>) ())

(define-interface <increasing-number-iterator> (<number-iterator>) ())

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
