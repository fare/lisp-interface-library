;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb
(module
 (:depends-on
  ("interface/interface" "interface/eq" "interface/order")))

(in-package :asdf/package)

(define-package :pure
  (:mix :cl :interface :fare-utils :asdf/driver :alexandria)
  (:nicknames #:pure-functional)
  (:export

   ;;; Iterators
   #:<fount> #:<sink>
   #:iterator #:next
   #:collector #:collect #:result
   #:flow
   #:<devnull>
   #:make-number-iterator
   #:<for-each> #:for-each #:for-each*

   ;; #:boolean-integer

   ;;; Emptyable
   #:<emptyable> #:empty #:empty-p

   ;;; Functional Maps and Containers: classes
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<number-map> #:<nm>
   #:<hash-table>
   #:<fmim> #:<encoded-key-map>

   ;; mixins
   #:<foldable-size-from-fold-left*>
   #:<map-empty-is-nil>
   #:<map-decons-from-first-key-value-drop>
   #:<map-divide/list-from-divide>
   #:<map-fold-right*-from-fold-left*>
   #:<map-for-each*-from-fold-left*>
   #:<map-join-from-fold-left*-insert>
   #:<map-join/list-from-join>
   #:<map-map/2-from-fold-left*-lookup-insert-drop>
   #:<map-update-key-from-lookup-insert-drop>
   #:<map-size<=n-p-from-decons>

   ;;; Trees
   #:<tree>
   #:node #:locate #:join
   #:left #:right #:leftmost #:rightmost
   ;;#:trie-lookup #:trie-fold-left #:trie-fold-right #:trie-leftmost #:trie-rightmost
   #:make-trie-leaf #:make-trie-skip #:make-trie-branch #:make-trie-head

   ;;; Functional Maps and Containers: Generic Functions
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

   #:check-invariant

   #:encode-key #:decode-key

   #|;; updatef
   #:updatef
   #:define-updatef-expander
   #:defupdatef
   #:define-updatef-function
   #:get-updatef-expansion
   #:updatef-function|#

   ;; linearize --- TODO: move to its own package?
   #:<linear> #:<linearized> #:<linearized-map>
   ))
