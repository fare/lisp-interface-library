;;;;; Stateful (mutable) mapping of keys to values

(uiop:define-package :lil/stateful/map
  (:use :closer-common-lisp
        :lil/core/definition
        :lil/interface/base)
  (:use-reexport
   :lil/interface/map
   :lil/interface/fold
   :lil/stateful/iterator
   :lil/stateful/collection)
  (:shadow #:<map>)
  (:export
   #:insert #:drop #:decons #:update-key #:map/2
   #:<map-copy-from-join-empty>
   #:<map-decons-from-first-key-value-drop>
   #:<map-divide-from-for-each*>
   #:<map-divide/list-from-divide>
   #:<map-empty-is-empty-object>
   #:<map-first-key-value-from-for-each*>
   #:<map-fold-left*-from-for-each*>
   #:<map-join-from-for-each*-lookup-insert>
   #:<map-join/list-from-join>
   #:<map-map/2-from-for-each*-lookup-insert-drop>
   #:<map-update-key-from-lookup-insert-drop>))
(in-package :lil/stateful/map)

(define-interface <map> (lil/interface/map:<map> <finite-collection> <fount> <sink>) ()
  (:abstract)
  (:generic> insert (map key value) (:in 1) (:values) (:out t)
   (:documentation "Modify the map to add a key-value pair,
replacing any previous association for this key.
Return no value."))
  (:generic> drop (map key) (:in 1) (:values value foundp) (:out t)
   (:documentation "Modify the map to drop the association corresponding to given key,
returning two values:
1- the value from the dropped association, and
2- a boolean that is true iff an association was found."))
  (:generic> decons (map) (:in 1) (:values emptyp key value) (:out t)
   (:documentation "Modify a map to drop its first association,
returning three values:
1- a boolean indicating whether the map was already empty.
2- a key
3- a value.
Which association is dropped is the same as per first-key-value."))
  (:generic> update-key (map key fun) (:in 1) (:values) (:out t)
   (:documentation "Update the association of a map for a given key
calling fun with the previous associated value and T if found, with NIL and NIL otherwise,
and return no values,
where fun will return two values,
the new value and a boolean,
the association being dropped if the boolean is NIL,
otherwise a new association being setup with the new value."))
  (:generic> map/2 (fun map1 map2) (:in 2 3) (:values) (:out t nil)
   (:documentation "Join two maps into the first one, after merging elements from MAP2.
Return no values.
For each key K present in either MAP1 or MAP2,
the function FUN is called with arguments K V1 F1 V2 F2 where
V1 and F1 are the value and found flag for MAP1, and
V2 and F2 are the value and found flag for MAP2,
and FUN returns value V and found flag F,
that correspond the lookup for K in the result.")))

#|
Instead of divide and divide/list and in the spirit of fold-left and fold-right,
we could have a
(defgeneric monoid-fold (i map m-null m-singleton m-join m-join/list))
|#

(define-interface <map-copy-from-join-empty> (<map>) ()
  (:abstract)
  (:documentation "Beware that join must be non-destructive of its second argument")
  (:method> copy (map)
    (let ((new (empty)))
      (join new map)
      new)))

;;; Mixins implementing simple cases for a lot of the above functions
(define-interface <map-decons-from-first-key-value-drop> (<map>) () (:abstract))
(define-interface <map-divide-from-for-each*> (<map>) () (:abstract))
(define-interface <map-divide/list-from-divide> (<map>) () (:abstract))
(define-interface <map-empty-is-empty-object>
    (<map> <empty-is-empty-object>) () (:abstract))
(define-interface <map-first-key-value-from-for-each*> (<map>) () (:abstract))
(define-interface <map-fold-left*-from-for-each*> (<map>) () (:abstract))
(define-interface <map-join-from-for-each*-lookup-insert> (<map>) () (:abstract))
(define-interface <map-join/list-from-join> (<map>) () (:abstract))
(define-interface <map-map/2-from-for-each*-lookup-insert-drop> (<map>) () (:abstract))
(define-interface <map-update-key-from-lookup-insert-drop> (<map>) () (:abstract))
