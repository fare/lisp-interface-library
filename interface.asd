;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :interface
  :description "LIL interface: abstract interfaces"
  :long-description "Basic support for generic data structure interfaces in Common Lisp"
  :depends-on (:fare-memoization :fare-utils :alexandria :closer-mop)
  :components
  (;;; Interface-Passing Style generic libraries
   (:module "interface"
    :components
    ((:file "package")
     (:file "interface" :depends-on ("package"))
     (:file "implicit" :depends-on ("interface"))
     (:file "box" :depends-on ("interface"))
     (:file "eq" :depends-on ("interface"))
     (:file "order" :depends-on ("eq"))
     (:file "iterator" :depends-on ("interface"))
     (:file "map-interface" :depends-on ("interface"))
     (:file "tree-interface" :depends-on ("map-interface" "order"))
     (:file "tree" :depends-on ("tree-interface"))))))
