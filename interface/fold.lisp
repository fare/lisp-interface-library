;;;;; Folding a data structure

(uiop:define-package :lil/interface/fold
  (:use :closer-common-lisp :uiop
   :core
   :lil/interface/base
   :lil/interface/size
   :lil/interface/group
   :lil/interface/empty
   :lil/interface/size
   :lil/interface/iterator)
  (:export
   #:<foldable> #:monoid-fold #:monoid-fold* #:fold-left #:fold-right
   #:fold-left* #:fold-right*
   #:<foldable-*-from> #:<foldable-monoid-fold-from-fold-left>
   #:<foldable-fold-right-from-fold-left> #:<foldable-for-each-from-fold-left>
   #:<foldable-size-from-fold-left>))
(in-package :lil/interface/fold)

(define-interface <foldable> (<for-each>) ()
  (:documentation "A type of objects that can be folded")
  (:abstract)
  (:generic> monoid-fold (<monoid> foldable function) (:in 2) (:values value)
   (:documentation "Fold the FOLDABLE according to a <MONOID> interface,
where each entry is mapped to the monoid using the provided function"))
  (:generic> monoid-fold* (<monoid> foldable function) (:in 2) (:values value)
   (:documentation "Fold the FOLDABLE according to a <MONOID> interface,
where each entry's values are mapped to the monoid using the provided function"))
  (:generic> fold-left (foldable function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional left to a notional right.
The function takes as parameters
an accumulated value followed by
a single value for the content of each iteration
and returns a new accumulated value."))
  (:generic> fold-left* (foldable function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional left to a notional right.
The function takes as parameters
an accumulated value followed by
as many values as make sense for the specific interface, i.e.
element for a set, element and count for a multi-set, key and value for a map,
and returns a new accumulated value."))
  (:generic> fold-right (foldable function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional right to a notional left.
The function takes as parameters
a single value for the content of each iteration
followed by an accumulated value
and returns a new accumulated value."))
  (:generic> fold-right* (map function seed) (:in 1) (:values value)
   (:documentation "Fold the FOLDABLE with a FUNCTION accumulating from the SEED,
iterating on the contents from a notional right to a notional left.
The function takes as parameters
as many values as make sense for the specific interface,
i.e. element for a set, element and count for a multi-set, key and value for a map,
followed by an accumulated value
and returns a new accumulated value.")))

(define-interface <foldable-*-from> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold* (<monoid> map function)
    (monoid-fold <monoid> map function))
  (:method> fold-left* (map function seed)
    (fold-left map function seed))
  (:method> fold-right* (map function seed)
    (fold-right map function seed))
  (:method> for-each* (map function)
    (for-each map function)))

(define-interface <foldable-monoid-fold-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> monoid-fold (<monoid> map fun)
    (fold-left
     map
     #'(lambda (acc entry) (op <monoid> acc (funcall fun entry)))
     (id <monoid>))))

(define-interface <foldable-fold-right-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> fold-right (map right-function seed)
    (funcall
     (fold-left
      map
      #'(lambda (f entry) #'(lambda (accumulator) (funcall f (funcall right-function entry accumulator))))
      #'identity)
     seed)))

(define-interface <foldable-for-each-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> for-each (map fun)
    (fold-left
     map
     #'(lambda (s e) (declare (ignore s)) (funcall fun e))
     nil)
    (values)))

(define-interface <foldable-size-from-fold-left> (<foldable>) ()
  (:abstract)
  (:method> size (map)
    (fold-left map #'(lambda (x e) (declare (ignore e)) (1+ x)) 0)))

