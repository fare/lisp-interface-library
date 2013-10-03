;;; Interface Passing Style : Monad : Maybe
(defpackage :lil/interface/monad/maybe
  (:use :cl :lil/interface/monad :lil/interface/definition)
  (:import-from :lil/interface/zero-plus
		#:<zero-plus>
		#:zero
		#:plus)
  (:import-from :lil/interface/base
		#:<empty-is-nil>
		#:empty
		#:empty-p)
  (:export #:<maybe>
	   #:maybe-or
	   #:<maybe-empty>
	   #:maybe.empty-interface))
(in-package :lil/interface/monad/maybe)

(define-interface <maybe-empty> (<empty-is-nil>)
  () (:singleton))

(define-interface <maybe> (<monad> <zero-plus>)
  ((empty-interface :initarg :empty-interface
		    :initform <maybe-empty>
		    :accessor maybe.empty-interface))
  (:parametric (&optional (empty-interface <maybe-empty>))
    (make-instance '<maybe> :empty-interface empty-interface))
  (:singleton)
  (:generic maybe-or (<maybe> value &rest values))
  (:method> result (value) value)
  (:method bind ((<m> <maybe>) mv mf)
    (if (empty-p (maybe.empty-interface <m>) mv)
	mv
	(funcall mf mv)))

  (:method zero ((<m> <maybe>))
    (empty (maybe.empty-interface <m>)))

  (:method plus ((<m> <maybe>) a b)
    (if (empty-p (maybe.empty-interface <m>) a)
	b
	a))
  (:method maybe-or ((<m> <maybe>) value &rest values)
     (if (not values)
	 value
	 (plus
	  <m> value
	  (if (rest values)
	      (apply #'maybe-or <m> values)
	      (first values)))))
  (:method> fail () (zero)))

(defmethod print-object ((object <maybe>) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~S" (type-of object)
	    (type-of (maybe.empty-interface object)))))
