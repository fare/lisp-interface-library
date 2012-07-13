;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping of keys to values

#+xcvb (module (:depends-on ("interface/interface" "pure/package")))

(in-package :pure)

(defclass <map> () ())

#| ;; Already defined in interface for boxes.
(defgeneric empty (<map>)
  (:documentation "Return an empty map"))

(defgeneric empty-p (<map> map)
  (:documentation "Return a boolean indicating whether the map was empty"))
|#

(defgeneric lookup (<map> map key)
  (:documentation "Lookup what map associates to a key,
return two values, the associated value and
a boolean that is true iff an association was found"))

(defgeneric insert (<map> map key value)
  (:documentation "Add a key-value pair to a map,
replacing any previous association for this key,
return a new map."))

(defgeneric drop (<map> map key)
  (:documentation "Drop from a map the association corresponding to given key,
returning three values:
a new map without that association,
the value from the dropped association,
and a boolean that is true iff an association was found."))

(defgeneric first-key-value (<map> map)
  (:documentation "Return three values:
a key, a value, and a boolean indicating
whether the map was already empty.
What first means here may depend on the particular map interface,
but generally means the element most easily accessible.
"))

(defgeneric decons (<map> map)
  (:documentation "Drop the first association from a map,
returning four values:
a new map, a key, a value, and a boolean indicating
whether the map was already empty.
What first means here may depend on the particular map interface,
but generally means the element most easily accessible.
"))

(defgeneric fold-left (<map> map f seed)
  (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons,
yielding association k_1 v_1 .. k_n v_n, and computing
(f (f ... (f (f seed k_1 v_1) k2 v_2) ... k_n-1 v_n-1) k_n v_n)"))

(defgeneric fold-right (<map> map f seed)
  (:documentation "Fold a map with a function,
by repeatedly deconstructing it as by decons,
yielding association k_1 v_1 .. k_n v_n, and computing
(f k_1 v_1 (f k2 v_2 (f ... (f k_n-1 v_n-1 (f k_n v_n seed))...)))"))

(defgeneric for-each (<iterator> iterator f)
  (:documentation "For every step in iterator, apply f to values"))

(defgeneric join (<map> map1 map2)
  (:documentation "Join two maps, returning a joined map.
Mappings from MAP1 override those from MAP2."))

(defgeneric divide (<map> map)
  (:documentation "Divide a map in two,
returning two maps MAP1 and MAP2 that each have strictly
fewer associations than MAP unless MAP is of size zero or one.
If MAP is of size one, then MAP1 is MAP and MAP2 is empty.
If MAP is of size zero, then both MAP1 and MAP2 are empty.
"))

(defgeneric size (<map> map)
  (:documentation "Size the number of elements in a map"))

(defgeneric join/list (<map> list)
  (:documentation "Join a list of maps,
returning a joined map where mappings from
earlier mappings override those from latter mappings."))

(defgeneric divide/list (<map> map)
  (:documentation "Divide a map in a list of several submaps and return that list,
such that merging those maps with join/list
will return a map similar to the original one,
that the returned list is empty iff the initial map is empty,
that the returned list is of length one iff the initial map is a singleton,
and that otherwise, each element of the list is non-empty."))

(defgeneric update-key (<map> map key fun)
  (:documentation "Update the association of a map for a given key and return a new map,
calling fun with the previous associated value and T if found, with NIL and NIL otherwise,
where fun will return two values,
the new value and a boolean,
the association being dropped if the boolean is NIL,
otherwise a new association being setup with the new value."))

(defgeneric map/2 (<map> fun map1 map2)
  (:documentation "Join two maps, returning a joined map.
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
(defclass map-simple-fold-right () ())
(defclass map-simple-for-each () ())
(defclass map-simple-size () ())
