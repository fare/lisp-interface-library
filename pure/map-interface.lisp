;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping of keys to values

#+xcvb (module (:depends-on ("interface/map-interface" "pure/collection")))

(in-package :pure)

(define-interface <map> (interface::<map> <finite-collection> <fount> <sink>) ()
  (:abstract)
  (:generic> insert (map key value) (:in 1) (:values map) (:out 0)
   (:documentation "Add a key-value pair to a map,
replacing any previous association for this key,
return a new map."))
  (:generic> drop (map key) (:in 1) (:values map value foundp) (:out 0)
   (:documentation "Drop from a map the association corresponding to given key,
returning three values:
a new map without that association,
the value from the dropped association,
and a boolean that is true iff an association was found."))
  (:generic> decons (map) (:in 1) (:values emptyp map key value) (:out 1)
   (:documentation "Drop an association from a map,
returning four values:
1- a boolean indicating whether the map was already empty.
2- a new map
3- a key
4- a value.
Which association is dropped is the same as per first-key-value."))
  (:generic> update-key (map key fun) (:in 1) (:values map) (:out 0)
   (:documentation "Update the association of a map for a given key and
return a new updated map
calling fun with the previous associated value and T if found, with NIL and NIL otherwise,
where fun will return two values,
the new value and a boolean,
the association being dropped if the boolean is NIL,
otherwise a new association being setup with the new value."))
  (:generic> map/2 (fun map1 map2) (:in 2 3) (:values map) (:out 0 nil)
   (:documentation "Join two maps, returning a joined map.
For each key K present in either MAP1 or MAP2,
the function FUN is called with arguments K V1 F1 V2 F2 where
V1 and F1 are the value and found flag for MAP1, and
V2 and F2 are the value and found flag for MAP2,
and FUN returns value V and found flag F,
that correspond the lookup for K in the result.")))

;;; Mixins implementing simple cases for a lot of the above functions
(define-interface <map-decons-from-first-key-value-drop> (<map>) () (:abstract))
(define-interface <map-divide/list-from-divide> (<map>) () (:abstract))
(define-interface <map-empty-is-nil> (<map> <empty-is-nil>) () (:abstract))
(define-interface <map-join-from-fold-left*-insert> (<map>) () (:abstract))
(define-interface <map-join/list-from-join> (<map>) () (:abstract))
(define-interface <map-map/2-from-fold-left*-lookup-insert-drop> (<map>) () (:abstract))
(define-interface <map-monoid-fold*-from-divide> (<foldable>) () (:abstract))
(define-interface <map-monoid-fold*-from-divide/list> (<foldable>) () (:abstract))
(define-interface <map-singleton-from-insert> (<map>) () (:abstract))
(define-interface <map-singleton-p-from-decons> (<map>) () (:abstract))
(define-interface <map-size<=n-p-from-decons> (<map>) () (:abstract))
(define-interface <map-update-key-from-lookup-insert-drop> (<map>) () (:abstract))
