;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map interface: alists.

#+xcvb (module (:depends-on ("interface/eq" "pure/map-interface")))

(in-package :pure)

(define-interface <alist>
    (<copy-is-identity>
     <map-empty-is-nil>
     <map-decons-from-first-key-value-drop>
     <map-update-key-from-lookup-insert-drop>
     <map-divide/list-from-divide>
     <map-map/2-from-fold-left-lookup-insert-drop>
     <map-join-from-fold-left-insert>
     <map-join/list-from-join>
     <map>)
  ((key-interface :type <eq> :initarg :key-interface :reader key-interface))
  (:parametric (&optional (eq <eql>)) (make-interface :key-interface eq))
  (:singleton))
