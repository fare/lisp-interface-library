;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :interface-transform
  :description "transform LIL interfaces"
  :long-description "Transforming stateful datastructures into pure datastructures and back"
  :depends-on (:pure-interfaces :stateful-interfaces)
  :components
  (;;; Interface-Passing Style generic libraries
   (:module "transform"
    :components
    ((:file "linearize")
     (:file "mutating")
     (:file "linearized-interfaces" :depends-on ("linearize"))
     (:file "mutating-interfaces" :depends-on ("mutating"))))))
