;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :interface
  :description "LIL interface: abstract interfaces"
  :long-description "Basic support for generic data structure interfaces in Common Lisp"
  :depends-on (:fare-memoization :xcvb-utils :closer-mop)
  :components
  (;;; Interface-Passing Style generic libraries
   (:module "interface"
    :components
    ((:file "package")
     (:file "interface" :depends-on ("package"))
     (:file "base" :depends-on ("interface"))
     (:file "box" :depends-on ("base"))
     (:file "eq" :depends-on ("base"))
     (:file "order" :depends-on ("eq"))
     (:file "iterator" :depends-on ("base"))
     (:file "set-interface" :depends-on ("base"))
     (:file "map-interface" :depends-on ("base"))
     (:file "tree-interface" :depends-on ("map-interface" "order"))
     (:file "tree" :depends-on ("tree-interface"))))))
