;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; stateful mapping where key is encoded.
#+xcvb (module (:depends-on ("pure/map")))

(in-package :stateful)

(define-interface <encoded-key-map> (<map>) ())

(define-interface <parametric-encoded-key-map> (<encoded-key-map>)
  ((base-interface :initarg :base-interface :reader base-interface)
   (key-encoder :initarg :key-encoder :reader key-encoder)
   (key-decoder :initarg :key-decoder :reader key-decoder))
  (:parametric (&key base-interface key-encoder key-decoder)
               (make-interface
                :base-interface base-interface
                :key-encoder key-encoder
                :key-decoder key-decoder)))

(defgeneric encode-key (<interface> plain-key))
(defgeneric decode-key (<interface> encoded-key))
