;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Stateful Datastructures

#+xcvb
(module
 (:depends-on
  ("interface/interface" "interface/eq" "interface/order")))

(in-package :cl)

(defpackage :stateful
  (:use :cl :interface :xcvb-utils)
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
   #:map-simple-empty #:empty-object
   #:map-simple-decons
   #:map-simple-update-key #:map-simple-join
   #:map-simple-join/list #:map-divide/list-from-divide
   #:map-simple-map/2 #:map-fold-right-from-fold-left
   #:map-for-each-from-fold-left #:map-size-from-fold-left
   #:map-divide-from-for-each
   #:map-fold-left-from-for-each #:map-first-key-value-from-for-each

   ;;; Stateful Maps and Containers: Generic Functions
   #:empty
   #:empty!
   #:empty-p
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

   #:balance-node #:rotate-node-left #:rotate-node-right

   ;; mutating --- TODO: move to its own package?
   #:<mutating> #:<mutating-map>
   #:<alist>
   ))
