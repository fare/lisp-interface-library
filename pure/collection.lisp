;;;;; Pure Collections

(uiop:define-package :lil/pure/collection
  (:use :closer-common-lisp
        :lil/core
        :lil/interface/base)
  (:use-reexport
   :lil/pure/empty
   :lil/interface/collection)
  (:shadow #:<finite-collection>)
  (:export
   #:<finite-collection> #:conj #:disj #:deconj #:restriction
   #:join #:divide #:join/list #:divide/list))
(in-package :lil/pure/collection)

(define-interface <finite-collection> (lil/interface/collection:<finite-collection> <empty!able>) ()
  (:abstract)
  (:generic> conj (collection entry) (:in 1) (:values collection) (:out 0)
   (:documentation "Add an ENTRY to a COLLECTION.
How the entry is combined with any existing entry of same key depends on the specific interface.
Return a new COLLECTION."))
  (:generic> disj (collection key) (:in 1) (:values collection entry foundp) (:out 0)
   (:documentation "Drop from the COLLECTION an entry matching the key, if any.
Return three values:
1- a new COLLECTION without that entry,
2- the ENTRY that was dropped if any, and
3- a boolean FOUNDP that is true iff an entry was found."))
  (:generic> deconj (collection) (:in 1) (:values collection entry foundp) (:out 0)
   (:documentation "Drop from the COLLECTION the first ENTRY, if any.
Return three values:
1- a new COLLECTION without that entry,
2- the ENTRY that was dropped if any, and
3- a boolean FOUNDP that is true iff an entry was found."))
  (:generic> restriction (predicate collection) (:in 2) (:values collection2) (:out 0)
   (:documentation "Restriction of the collection to the keys that verify the predicate"))
  (:generic> join (collection1 collection2) (:in 1 2) (:values collection1) (:out 0 nil)
   (:documentation "Join two collections, returning a new joined collection.
How entries are combined depends on the specific interface.
Typically, entries from the first override entries from the second."))
  (:generic> divide (collection) (:in 1) (:values collection1 collection2) (:out 1 0)
   (:documentation "Divide a COLLECTION in two,
returning two collections COLLECTION1 and COLLECTION2 that each have strictly
fewer entries than COLLECTION unless COLLECTION is of size zero or one,
at which point the first result COLLECTION1 is empty."))
  (:generic> join/list (list) #|(:in #|((1 list))|#) (:values collection) (:out 0)|#
   (:documentation "Join a list of COLLECTIONS, returning a new joined COLLECTION.
earlier entries override those from latter entries.
How entries are combined depends on the specific interface.
Typically, entries from earlier collections override entries from latter ones."))
  (:generic> divide/list (collection) #|(:in 1) (:values list) (:out (or null (cons map list)))|#
   (:documentation "Divide a collection in a list of several subcollections and return that list,
such that merging those collections with join/list
will return a collection similar to the original one,
that the returned list is empty iff the initial collection is empty,
that the returned list is of length one iff the initial collection is a singleton,
and that otherwise, each element of the list is non-empty.")))
