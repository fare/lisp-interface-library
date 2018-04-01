
(uiop:define-package :lil/core/interface
  (:use :closer-common-lisp :closer-mop
        :lil/core/utility :lil/core/metaclass))

(in-package :lil/core/interface)

(define-interface <interface> () ()
  (:abstract)
  (:documentation "An interface, encapsulating an algorithm"))

(defmethod shared-initialize :before ((instance <interface>) slot-names &key &allow-other-keys)
  (declare (ignorable slot-names))
  (when (interface-abstract-p (class-of instance))
    (error "Trying to instantiate abstract interface ~S" (type-of instance))))
