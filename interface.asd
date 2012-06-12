;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :interface
  :description "LIL interface: abstract interfaces"
  :long-description "Basic support for generic data structure interfaces in Common Lisp"
  :depends-on (:fare-memoization :alexandria)
  :components
  (;;; Interface-Passing Style generic libraries
   (:module "interface"
    :components
    ((:file "interface")
     (:file "box" :depends-on ("interface"))
     (:file "eq" :depends-on ("interface"))
     (:file "order" :depends-on ("eq"))))))
