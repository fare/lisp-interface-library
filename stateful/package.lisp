;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Stateful Datastructures

#+xcvb
(module
 (:depends-on
  ("interface/interface" "interface/eq" "interface/order")))

(in-package :asdf/package)

(define-package :stateful
  (:mix :cl :interface :fare-utils :asdf/driver :alexandria)
  (:export

   ;;; Base interfaces
   #:<type> #:check-invariant #:convert
   #:<copyable> #:copy
   #:<emptyable> #:empty #:empty-p
   #:<foldable> #:monoid-fold #:monoid-fold* #:fold-left #:fold-right
   #:fold-left* #:fold-right* #:for-each #:for-each*
   #:<makeable> #:make #:create #:contents
   #:<sizable> #:size #:size<=n-p
   #:<finite-collection> #:get-entry #:has-key-p #:first-entry #:entry-values
                             #:singleton-p #:singleton #:singleton*
   #:<encoded-key-collection> #:<parametric-encoded-key-collection> #:encode-key #:decode-key

   ;;; Iterators
   #:<fount> #:<sink>
   #:iterator #:next
   #:collector #:collect #:result
   #:flow
   #:<devnull>
   #:make-number-iterator
   #:<for-each> #:for-each #:for-each*

   ;;; Trees
   #:<tree>
   #:node #:locate #:join
   #:left #:right #:leftmost #:rightmost

   ;;; Stateful Maps and Containers: classes
   #:<map>
   #:<binary-tree> #:<avl-tree>
   #:<number-map> #:<nm>
   #:<hash-table>
   #:<encoded-key-map>
   #:<map-copy-from-join-empty>
   #:<map-empty-is-empty-object> #:empty-object
   #:<map-decons-from-first-key-value-drop>
   #:<map-update-key-from-lookup-insert-drop>
   #:<map-join-from-for-each*-lookup-insert>
   #:<map-join/list-from-join>
   #:<map-divide/list-from-divide>
   #:<map-map/2-from-for-each*-lookup-insert-drop>
   #:<map-foldable-from-*>
   #:<map-fold-left*-from-each*>
   #:<map-fold-right*-from-fold-left*>
   #:<map-for-each*-from-fold-left*>
   #:<map-size-from-fold-left>
   #:<map-divide-from-for-each*>
   #:<map-first-key-value-from-for-each*>

   ;;; Stateful Maps and Containers: Generic Functions
   #:empty!
   #:lookup
   #:insert
   #:drop
   #:first-key-value
   #:decons
   #:fold-left #:fold-right #:fold-left* #:fold-right*
   #:join
   #:divide
   #:size
   #:join/list
   #:divide/list
   #:update-key
   #:map/2
   #:convert

   #:balance-node #:rotate-node-left #:rotate-node-right

   ;;; Queues
   #:<queue> #:enqueue #:dequeue #:enqueue-first #:enqueue-last #:enqueue-many #:dequeue-all
   #:<fifo-queue> #:<lifo-queue>

   ;; mutating --- TODO: move to its own package?
   #:<mutating> #:<mutating-map>
   #:<alist>
   ))
