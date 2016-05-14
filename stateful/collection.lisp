;;;;; Stateful collections

(uiop:define-package :lil/stateful/collection
  (:use :closer-common-lisp
        :core
        :lil/interface/base)
  (:use-reexport
   :lil/stateful/empty
   :lil/interface/collection)
  (:shadow #:<finite-collection>)
  (:export
   #:<finite-collection> #:conj #:disj #:deconj #:restriction
   #:join #:divide #:join/list #:divide/list))
(in-package :lil/stateful/collection)

(define-interface <finite-collection> (lil/interface/collection:<finite-collection> <empty!able>) ()
  (:abstract)
  (:generic> conj (collection entry) (:in 1) (:values) (:out t)
   (:documentation "Add an ENTRY to a COLLECTION.
How the entry is combined with any existing entry of same key depends on the specific interface.
Return a new COLLECTION."))
  (:generic> disj (collection key) (:in 1) (:values entry foundp) (:out t)
   (:documentation "Drop from the COLLECTION an entry matching the key, if any.
Return two values:
1- the ENTRY that was dropped if any, and
2- a boolean FOUNDP that is true iff an entry was found."))
  (:generic> deconj (collection) (:in 1) (:values entry foundp) (:out t)
   (:documentation "Drop from the COLLECTION the first ENTRY, if any.
Return two values:
1- the ENTRY that was dropped if any, and
2- a boolean FOUNDP that is true iff an entry was found."))
  (:generic> restriction (predicate collection) (:in 2) (:values) (:out t)
   (:documentation "Restriction of the collection to the keys that verify the predicate"))
  (:generic> join (collection1 collection2) (:in 1 2) (:values) (:out t t)
   (:documentation "Join two collections into the first one.
How entries are combined depends on the specific interface.
Typically, entries from the first override entries from the second."))
  (:generic> divide (collection) (:in 1) (:values collection2 collection) (:out 1 0)
   (:documentation "Divide a COLLECTION in two,
returning two maps COLLECTION2 and COLLECTION (eq to the COLLECTION argument)
that each have strictly fewer associations than COLLECTION originally had,
unless COLLECTION is of size zero or one, at which point COLLECTION2 is empty."))
  (:generic> join/list (list) #|(:in #|((1 list))|#) (:values collection) (:out 0)|#
   (:documentation "Join a list of COLLECTIONS, returning a new joined COLLECTION.
How entries are combined depends on the specific interface.
For instance, for maps, latter entries override those from earlier entries;
in other cases, entries from earlier collections may override entries from latter ones.
If the list is empty, a new empty collection is returned;
otherwise, the first collection is returned,
that has been updated with any additional mappings,
whereas the state of other maps is not specified (see method documentation)."))
  (:generic> divide/list (collection) #|(:in 1) (:values list) (:out t #|((0 list))|#)|#
   (:documentation "Divide a collection in a list of several subcollections and return that list,
such that merging those collections with join/list
will return a collection similar to the original one,
that the returned list is empty iff the initial collection is empty,
that the returned list is of length one iff the initial collection is a singleton,
that otherwise, each element of the list is non-empty,
and the first one is EQ to the original collection.")))
