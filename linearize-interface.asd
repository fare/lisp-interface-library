;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :linearize-interface
  :description "linearize LIL interfaces: from stateful to pure datastructure"
  :long-description "Transforming stateful datastructures into pure datastructures"
  :depends-on (:pure-interfaces :stateful-interfaces)
  :components
  (;;; Interface-Passing Style generic libraries
   (:module "linearize"
    :components
    ((:file "linearize")
     (:file "base" :depends-on ("linearize"))
     (:file "map" :depends-on ("base"))))))
