;;;;; Stateful hash-tables (thin wrapper over the implementation)

(uiop:define-package :lil/stateful/hash-table
  (:use :closer-common-lisp
        :lil/core/definition
        :lil/interface/base
        :lil/stateful/tree
        :lil/stateful/encoded-key-map)
  (:use-reexport
   :lil/interface/eq
   :lil/stateful/map)
  (:export
   #:<hash-table> #:hashmap-interface #:bucketmap-interface))
(in-package :lil/stateful/hash-table)

(define-interface <hash-table>
    (<map-copy-from-join-empty>
     <map-decons-from-first-key-value-drop>
     <map-divide-from-for-each*>
     <map-divide/list-from-divide>
     <map-first-key-value-from-for-each*>
     <map-foldable-from-*>
     <map-fold-left*-from-for-each*>
     <map-fold-right*-from-fold-left*>
     <map-join-from-for-each*-lookup-insert>
     <map-join/list-from-join>
     <map-map/2-from-for-each*-lookup-insert-drop>
     <map-monoid-fold*-from-fold-left*>
     <map-update-key-from-lookup-insert-drop>
     <sizable-size<=n-p-from-size>
     <map>)
  ((key-interface :reader key-interface :initarg :key-interface))
  ;; we would default to <eql>, but there is no standard hash function associated to it, for
  ;; there is no standard way to re-hash the table after a GC that would disturb pointer values.
  ;; So instead we default to <equal>.
  (:parametric (&optional (key <equal>) (value <any>))
     (make-interface :key-interface key :value-interface value))
  (:singleton)
  (:documentation "stateful hash table"))
