;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; Trivial functional map interface: alists.

#+xcvb (module (:depends-on ("interface/eq" "pure/map-interface")))

(in-package :pure)

(define-interface <alist>
    (<copy-is-identity>
     <map-empty-is-nil>
     <map-decons-from-first-key-value-drop>
     <map-divide/list-from-divide>
     <map-foldable-from-*>
     <map-join-from-fold-left*-insert>
     <map-join/list-from-join>
     <map-map/2-from-fold-left*-lookup-insert-drop>
     <map-monoid-fold*-from-fold-left*>
     <map-update-key-from-lookup-insert-drop>
     <map-singleton-from-insert>
     <map-singleton-p-from-decons>
     <map>
     <monoid>)
  ((key-interface :type <eq>
    :reader key-interface :initarg :key-interface)
   (value-interface :type <type>
    :reader value-interface :initarg :value-interface))
  (:parametric (&optional (key-interface <eql>) (value-interface <any>))
     (make-interface :key-interface key-interface :value-interface value-interface))
  (:singleton))
