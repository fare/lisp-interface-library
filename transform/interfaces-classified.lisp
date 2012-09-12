;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Interfaces to Classes: Examples

#+xcvb (module (:depends-on ("transform/classify" "pure/map-interface")))

(defpackage :number-map (:use :xcvb-utils :cl))
(in-package :number-map)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(interface:define-classified-interface-class
  >map< (value-box) stateful:<map>
  ((interface :initform stateful:<number-map> :allocation :class))
  (:extract-interface stateful:<number-map>) (:interface-keyword nil))
