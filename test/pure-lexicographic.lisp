(uiop:define-package :lil/test/pure-lexicographic
  (:use :pure :lil/test/base
        :lil/core/metaclass
        :lil/interface/base :lil/interface/eq :lil/interface/order
        :lil/pure/lexicographic :lil/pure/sequence
        :lil/pure/iterator :lil/pure/iterator
        :cl :uiop :fare-utils :hu.dwim.stefil)
  (:export #:test-pure-lexicographic))

(in-package :lil/test/pure-lexicographic)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-pure-lexicographic
            :in test-suite
            :documentation "Testing pure lexicographic sequences"))

;; TODO: move that to the main library...
(define-interface <list-of> (<sequence> <fount>) ()
  (:method> iterator (l) l)
  (:method> next (l) (values (and l t) (cdr l) (car l))))

(define-interface <lexicographic-number-sequence> (<lexicographic> <list-of>)
    ((value-interface :initform <number>))
  (:singleton))

(defparameter *splsl*
  '(()
    (0)
    (0 1 2 3)
    (1)
    (1 2)
    (1 2 0)
    (1 2 1)
    (1 2 2)
    (1 3)
    (2 3 4)
    (9 9)))

(deftest test-pure-lexicographic-sequence ()
  (is (equal (sort (copy-list *splsl*) (lambda (x y) (order< <lexicographic-number-sequence> x y)))
             *splsl*)))
