;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; stateful mapping where key is encoded.
#+xcvb (module (:depends-on ("stateful/map-interface")))

(in-package :stateful)

(define-interface <encoded-key-map>
    (<encoded-key-collection> <map-foldable-from-*> <map>) ()
  (:abstract))

(define-interface <parametric-encoded-key-map> (<parametric-encoded-key-collection> <encoded-key-map>) ()
  (:parametric (&key base-interface key-encoder key-decoder
		     key-interface value-interface)
               (make-interface
                :base-interface base-interface
		:key-interface key-interface
		:value-interface value-interface
                :key-encoder key-encoder
                :key-decoder key-decoder)))
