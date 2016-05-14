;;;;; Interfaces from elementary group theory: magma, monoid, semi-group, group, ring, etc.

(uiop:define-package :lil/interface/group
  (:use :closer-common-lisp :lil/core/definition :lil/interface/base)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   ;;; Algebra
   #:<magma> #:op
   #:<semigroup> #:op/list
   #:<monoid> #:id
   #:<group> #:inverse
   #:<semiring> #:multiplicative-operation))
(in-package :lil/interface/group)

(define-interface <magma> (<type>) ()
  (:abstract)
  (:generic> op (x y) (:values z) (:in 1 2) (:out 0))) ; x * y
(define-interface <semigroup> (<magma>) () ;; associativity: (== (op (op x y) z) (op x (op y z)))
  (:abstract)
  (:generic> op/list (list) (:values x))) ; if not a monoid, the list must be non-empty
(define-interface <identity> (<magma>) () ;; identity: (== x (op id x) (op x id))
  (:abstract)
  (:generic> id () (:values id) (:out 0)))
(define-interface <monoid> (<semigroup> <identity>) () ;; associativity, identity
  (:abstract))

#|
(define-interface <quasigroup> (<magma>) () ;; division
  (:abstract)
  (:generic> left-division (x y)) ; x \ y  (== (op x (left-division x y)) y)
  (:generic> right-division (x y))) ; x / y  (== x (op (right-division x y) y))
(define-interface <loop> (<quasigroup> <identity>) () ;; division, identity
|#
(define-interface <group> (<monoid> #|<quasigroup>|#) () ;; associativity, identity, division
  (:generic> inverse (x)))
(define-interface <semiring> (<group>) ()
  (:generic> multiplicative-operation ()))

(define-interface <zero-plus> (<monoid>) ;; an additively-notated monoid.
  ()
  (:abstract)
  (:generic> zero () (:values zero) (:out 0))
  (:generic> plus (a b) (:values c) (:in 1 2) (:out 0))
  (:method> id () (zero))
  (:method> op (a b) (plus a b)))
