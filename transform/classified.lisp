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
  (:use :asdf/driver :fare-utils :cl)
  (:import-from :interface
    #:define-classified-interface-class
    #:class-interface
    #:interface ;; for the slot name
    #:box!)
  (:export
   #:object-box
   #:>map< #:>number-map<
   #:>queue< #:>simple-fifo-queue<
   ;; Collections
   #:get-entry #:has-key-p #:first-entry #:entry-values
   #:singleton-p #:singleton #:singleton* #:collection-entries #:from-entries
   ;; map functions
   #:lookup #:insert #:drop #:first-key-value #:decons
   #:fold-left #:fold-right #:fold-left* #:fold-right* #:for-each #:for-each*
   #:join #:divide #:join/list #:divide/list #:map/2
   #:encode-key #:decode-key #:update-key
   #:check-invariant #:size #:convert
   ;;; Queue functions
   #:enqueue #:dequeue #:enqueue-first #:enqueue-last #:enqueue-many #:dequeue-all
   ))

(defpackage :posh
  (:documentation "Pure Object-Oriented Structure Hierarchy, classy APIs for pure LIL data structure")
  (:use :asdf/driver :fare-utils :cl)
  (:import-from :interface
    #:define-classified-interface-class
    #:class-interface
    #:interface ;; for the slot name
    #:simple-value-box)
  (:export
   #:object-box
   #:>map< #:>hash-table< #:>number-map<
   #:>queue< #:>simple-fifo-queue<
   ;; Collections
   #:get-entry #:has-key-p #:first-entry #:entry-values
   #:singleton-p #:singleton #:singleton* #:collection-entries #:from-entries
   ;; map functions
   #:lookup #:insert #:drop #:first-key-value #:decons
   #:fold-left #:fold-right #:fold-left* #:fold-right* #:for-each #:for-each*
   #:join #:divide #:join/list #:divide/list #:map/2
   #:encode-key #:decode-key #:update-key
   #:check-invariant #:size #:convert
   ;;; Queue functions
   #:enqueue #:dequeue #:enqueue-first #:enqueue-last #:enqueue-many #:dequeue-all))

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


(define-classified-interface-class
  >queue< (object-box) stateful:<queue>
  ((interface :initarg :interface))
  (:interface-argument (stateful:<queue> stateful:<queue>)))

(define-classified-interface-class
  >simple-fifo-queue< (>queue<) stateful:<simple-fifo-queue>
  ((interface :initform stateful:<simple-fifo-queue> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -simple-fifo-queue))



(in-package :posh)

(defclass object-box (simple-value-box)
  ((interface :reader class-interface)))

(define-classified-interface-class
  >map< (object-box) pure:<map>
  ((interface :initarg :interface))
  (:interface-argument (pure:<map> pure:<map>)))

(define-classified-interface-class
  >hash-table< (>map<) pure:<hash-table>
  ((interface :initform pure:<hash-table> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -hash-table))

(define-classified-interface-class
  >number-map< (>map<) pure:<number-map>
  ((interface :initform pure:<number-map> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -number-map))


(define-classified-interface-class
  >queue< (object-box) pure:<queue>
  ((interface :initarg :interface))
  (:interface-argument (pure:<queue> pure:<queue>)))

(define-classified-interface-class
  >simple-fifo-queue< (>queue<) pure:<simple-fifo-queue>
  ((interface :initform pure:<simple-fifo-queue> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -queue))

