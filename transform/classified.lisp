;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Interfaces to Classes: Examples

#+xcvb (module (:depends-on ("transform/classify" "pure/map-interface" "stateful/tree-interface")))

(defpackage :classified
  (:use :xcvb-utils :cl)
  (:import-from :interface
    #:define-classified-interface-class
    #:class-interface
    #:interface ;; for the slot name
    ))

(in-package :classified)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(define-classified-interface-class
  >map< (object-box) stateful:<map>
  ((interface :initarg :interface))
  (:interface-argument stateful:<map>))

(defpackage :classified-number-map
  (:use :xcvb-utils :cl)
  (:import-from :interface
    #:define-classified-interface-class
    #:class-interface))

(in-package :classified-number-map)

(define-classified-interface-class
  >nm< (object-box) stateful:<number-map>
  ((interface :initform stateful:<number-map> :allocation :class))
  (:interface-keyword nil) (:extract-interface stateful:<number-map>))
