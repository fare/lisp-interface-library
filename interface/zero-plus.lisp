;;; Interface Passing Style : Zero/plus

(defpackage :lil/interface/zero-plus
  (:nicknames :drewc.org/ips/zero-plus)
  (:use :cl :lil/interface/definition #:lil/interface/base)
  (:export #:<zero-plus> #:zero #:plus))
(in-package :lil/interface/zero-plus)

(define-interface <zero-plus> (<monoid>)
  ()
  (:abstract)
  (:generic> zero () (:values zero) (:out 0))
  (:generic> plus (a b) (:values c) (:in 1 2) (:out 0))
  (:method> id () (zero))
  (:method> op (a b) (plus a b)))
