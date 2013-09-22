;;; Interface Passing Style : Monad : Maybe
(defpackage :interface/monad/maybe
  (:nicknames :drewc.org/ips/monad/maybe)
  (:use :cl :interface/monad)
  (:import-from :interface/zero-plus
		#:<zero-plus>
		#:zero
		#:plus)
  (:import-from :interface
		#:<empty-is-nil>
		#:empty
		#:empty-p)
  (:export #:<maybe>
	   #:maybe-or
	   #:<maybe-empty>
	   #:maybe.empty-interface))
(in-package :interface/monad/maybe)

(interface:define-interface <maybe-empty> (<empty-is-nil>)
  () (:singleton))

(interface:define-interface <maybe> (<monad> <zero-plus>)
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
