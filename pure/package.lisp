;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Interfaces for Pure Functional Data-Structures

#+xcvb
(module
 (:depends-on
  ("package" "interface/interface" "interface/eq" "interface/order")))

(in-package :cl)

(defpackage :pure
  (:nicknames #:pure-functional)
  (:use :cl :interface :order :eq :alexandria)
  (:export

   ;;; Trees
   #:<tree>
   #:node #:locate #:join
   #:left #:right #:leftmost #:rightmost

   ;;; Functional Maps and Containers: classes
   #:<map> #:<alist>
   #:<binary-tree> #:<avl-tree>
   #:<number-map> #:<nm>
   #:<hash-table>
   #:<fmim> #:<encoded-key-map>
   #:map-simple-empty #:map-simple-decons
   #:map-simple-update-key #:map-simple-join
   #:map-simple-join/list #:map-simple-divide/list
   #:map-simple-map/2 #:map-simple-fold-right
   #:map-simple-for-each #:map-simple-size

   ;;; Functional Maps and Containers: Generic Functions
   #:empty
   #:empty-p
   #:lookup
   #:insert
   #:drop
   #:first-key-value
   #:decons
   #:fold-left
   #:fold-right
   #:for-each
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

   ;; updatef
   #:updatef
   #:define-updatef-expander
   #:defupdatef
   #:define-updatef-function
   #:get-updatef-expansion
   #:updatef-function
   ))
