;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb
(module
 (:depends-on
  ("interface/interface" "interface/eq" "interface/order")))

(in-package :cl)

(defpackage :pure
  (:nicknames #:pure-functional)
  (:use :cl :interface :order :eq :xcvb-utils)
  (:export

   ;;; Iterators
   #:<fount> #:<sink>
   #:iterator #:next
   #:collector #:collect #:result
   #:flow
   #:<devnull>
   #:make-number-iterator
   #:<for-each> #:for-each

   ;; #:boolean-integer

   ;;; Emptyable
   #:<emptyable> #:empty #:empty-p

   ;;; Functional Maps and Containers: classes
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<number-map> #:<nm>
   #:<hash-table>
   #:<fmim> #:<encoded-key-map>
   #:map-simple-empty #:map-simple-decons
   #:map-simple-update-key #:map-simple-join
   #:map-simple-join/list
   #:map-simple-map/2
   #:map-divide/list-from-divide
   #:map-fold-right-from-fold-left
   #:map-for-each-from-fold-left #:map-size-from-fold-left
   #:map-size<=n-p-from-decons

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
   #:fold-left
   #:fold-right
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
