;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful mapping of keys to values

#+xcvb (module (:depends-on ("interface/interface" "stateful/package")))

(in-package :stateful)

(define-interface <map> (<fount> <sink>) ())

(defgeneric empty! (<map> map)
  (:documentation "Clear the map and make it empty. Return no value."))

(defgeneric insert (<map> map key value)
  (:documentation "Modify the map to add a key-value pair,
replacing any previous association for this key.
Return no value."))

(defgeneric drop (<map> map key)
  (:documentation "Modify the map to drop the association corresponding to given key,
returning two values:
1- the value from the dropped association, and
2- a boolean that is true iff an association was found."))

(defgeneric decons (<map> map)
  (:documentation "Modify a map to drop its first association,
returning three values:
1- a boolean indicating whether the map was already empty.
2- a key
3- a value.
Which association is dropped is the same as per first-key-value."))

(defgeneric join (<map> map1 map2)
  (:documentation "Join two maps into the first one.
Mappings from MAP1 override those from MAP2.
The state of MAP2 after the join is not specified (see method documentation).
Return MAP1."))

(defgeneric divide (<map> map)
  (:documentation "Divide a MAP in two,
returning two maps MAP2 and MAP (eq to the MAP argument)
that each have strictly fewer associations than MAP originally had,
unless MAP is of size zero or one, at which point MAP2 is empty.
"))

(defgeneric join/list (<map> list)
  (:documentation "Join a list of maps,
returning a joined map where mappings from
later mappings override those from earlier mappings.
If the list is empty, a new empty map is returned;
otherwise, the first list is returned,
that has been updated with any additional mappings,
whereas the state of other maps is not specified (see method documentation)."))

(defgeneric divide/list (<map> map)
  (:documentation "Divide a map in a list of several submaps and return that list,
such that merging those maps with join/list
will return a map similar to the original one,
that the returned list is empty iff the initial map is empty,
that the returned list is of length one iff the initial map is a singleton,
and that otherwise, each element of the list is non-empty
and the first one is EQ to the original map."))

(defgeneric update-key (<map> map key fun)
  (:documentation "Update the association of a map for a given key
calling fun with the previous associated value and T if found, with NIL and NIL otherwise,
and return no values,
where fun will return two values,
the new value and a boolean,
the association being dropped if the boolean is NIL,
otherwise a new association being setup with the new value."))

(defgeneric map/2 (<map> fun map1 map2)
  (:documentation "Join two maps, returning MAP1 after merging elements from MAP2.
For each key K present in either MAP1 or MAP2,
the function FUN is called with arguments K V1 F1 V2 F2 where
V1 and F1 are the value and found flag for MAP1, and
V2 and F2 are the value and found flag for MAP2,
and FUN returns value V and found flag F,
that correspond the lookup for K in the result."))

#|
Instead of divide and divide/list and in the spirit of fold-left and fold-right,
we could have a
(defgeneric monoid-fold (i map m-null m-singleton m-join m-join/list))
|#

;;; Mixins implementing simple cases for a lot of the above functions
(defclass map-simple-empty () ())
(defclass map-simple-decons () ())
(defclass map-simple-update-key () ())
(defclass map-simple-join () ())
(defclass map-simple-join/list () ())
(defclass map-simple-divide/list () ())
(defclass map-simple-map/2 () ())
(defclass map-divide-from-for-each () ())
(defclass map-fold-left-from-for-each () ())
(defclass map-first-key-value-from-for-each () ())
