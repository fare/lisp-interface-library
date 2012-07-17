;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; "Fast Mergable Integer Maps"
;;; See article of same name by Chris Okasaki & Andrew Gill, 1998
;;; http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
;;; Under the hood: Big Endian Patricia Trees (Tries).
;;; Note however that in our API, what they call "merge" is called "join".

#+xcvb
(module
 (:depends-on
  ("interface/interface" "pure/package" "pure/map" "pure/alist" "pure/tree")))

(in-package :pure)

(define-interface <fmim>
    (<tree> <map>
     map-simple-empty map-simple-decons map-simple-update-key
     map-simple-map/2 map-simple-join/list
     map-size-from-fold-left map-for-each-from-fold-left
     map-simple-divide/list)
  ()
  (:singleton)
  (:documentation "Fast Merge Integer Maps"))

;;; (big-endian) patricia tree (aka trie)
(defclass trie-head (simple-value-box)
  ((height
    :type fixnum
    :initform 0
    :initarg :height
    :reader node-height)))

(defclass trie-node () ())

(defclass trie-skip (trie-node simple-value-box)
  ((prefix-bits
    :type (integer 0 *)
    :initarg :prefix-bits
    :reader node-prefix-bits)
   (prefix-length
    :type fixnum
    :initarg :prefix-length
    :reader node-prefix-length)))

(defclass trie-branch (trie-node interface::binary-branch) ())

(defclass full-trie-branch (trie-branch) ())
;;; Not needed: position tells us! (defclass trie-leaf (trie-node simple-value-box) ())

