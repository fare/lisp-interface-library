;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Interfaces to Classes: Examples

#+xcvb (module (:depends-on ("transform/classify"
                             "pure/map-interface"
                             "pure/hash-table-interface"
                             "pure/tree-interface"
                             "stateful/tree-interface")))

(in-package :cl)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defpackage :classy
  (:documentation "traditional classy APIs for LIL data structure")
  (:use :xcvb-utils :cl)
  (:import-from :interface
    #:define-classified-interface-class
    #:class-interface
    #:interface ;; for the slot name
    #:box!)
  (:export
   #:object-box
   #:>map<
   #:>number-map<
   ;; map interface functions
   #:lookup
   #:insert
   #:drop
   #:first-key-value
   #:decons
   #:fold-left #:fold-right #:fold-left* #:fold-right* #:for-each #:for-each*
   #:join
   #:divide
   #:size
   #:join/list
   #:divide/list
   #:update-key
   #:map/2
   #:convert
   #:check-invariant
   #:encode-key #:decode-key))

(defpackage :posh
  (:documentation "Pure Object-Oriented Structure Hierarchy, classy APIs for pure LIL data structure")
  (:use :xcvb-utils :cl)
  (:import-from :interface
    #:define-classified-interface-class
    #:class-interface
    #:interface ;; for the slot name
    #:simple-value-box)
  (:export
   #:object-box
   #:>map<
   #:>hash-table<
   #:>number-map<
   ;; map interface functions
   #:lookup
   #:insert
   #:drop
   #:first-key-value
   #:decons
   #:fold-left #:fold-right #:fold-left* #:fold-right* #:for-each #:for-each*
   #:join
   #:divide
   #:size
   #:join/list
   #:divide/list
   #:update-key
   #:map/2
   #:convert
   #:check-invariant
   #:encode-key #:decode-key))

(in-package :classy)

(defclass object-box (box!)
  ((interface :reader class-interface)))

(define-classified-interface-class
  >map< (object-box) stateful:<map>
  ((interface :initarg :interface))
  (:interface-argument (stateful:<map> stateful:<map>)))

(define-classified-interface-class
  >number-map< (>map<) stateful:<number-map>
  ((interface :initform stateful:<number-map> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -number-map))

(in-package :posh)

(defclass object-box (simple-value-box)
  ((interface :reader class-interface)))

(define-classified-interface-class
  >map< (object-box) stateful:<map>
  ((interface :initarg :interface))
  (:interface-argument (stateful:<map> stateful:<map>)))

(define-classified-interface-class
  >hash-table< (>map<) pure:<hash-table>
  ((interface :initform pure:<hash-table> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -hash-table))

(define-classified-interface-class
  >number-map< (>map<) pure:<number-map>
  ((interface :initform pure:<number-map> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -number-map))
