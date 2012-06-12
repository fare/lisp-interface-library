;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :fare-utils
  :description "Basic functions and macros, interfaces, pure and stateful datastructures"
  :long-description "fare-utilities is a small collection of utilities.
It contains a lot of basic everyday functions and macros,
but also a library of pure and stateful datastructures,
and Lisp extensions for memoization and reader interception."
  :depends-on ((:version :asdf "2.019") :fare-memoization)
  :components
  ((:file "package")

   ;;; Utilities wrt Lisp
   (:module "base"
    :depends-on ("package")
    :components
    ((:file "utils")
     (:file "strings" :depends-on ("utils"))
     (:file "symbols" :depends-on ("strings"))
     (:file "macros" :depends-on ("symbols"))
     (:file "lists" :depends-on ("macros"))
     (:file "packages" :depends-on ("lists"))
     (:file "objects" :depends-on ("macros"))
     (:file "streams" :depends-on ("utils"))
     (:file "hash-tables" :depends-on ("macros"))
     (:file "more-strings" :depends-on ("strings" "streams"))
     (:file "parse-cl-syntax" :depends-on ("macros"))))

   ;;; Utilities wrt Lisp
   (:module "filesystem"
    :depends-on ("base")
    :components
    ((:file "pathnames")
     (:file "files")
     (:file "atomic")))

   ;;; Half-baked stuff
   (:module "unbaked"
    :depends-on ("base")
    :components
    ((:file "msv"))) ; Magic Special Variables

   ;;; Interface-Passing Style generic libraries
   (:module "interface"
    :depends-on ("base")
    :components
    ((:file "interface")
     (:file "box" :depends-on ("interface"))
     (:file "eq" :depends-on ("interface"))
     (:file "order" :depends-on ("eq"))))

   ;;; IPS pure functional datastructures
   (:module "pure"
    :depends-on ("interface")
    :components
    ((:file "package")
     (:file "map" :depends-on ("package"))
     (:file "updatef" :depends-on ("package"))
     (:file "updatef-expanders" :depends-on ("updatef"))
     (:file "alist" :depends-on ("map"))
     (:file "tree" :depends-on ("map" "alist"))
     (:file "hash-table" :depends-on ("tree"))
     (:file "fmim" :depends-on ("map" "tree"))
     (:file "encoded-key-map" :depends-on ("map"))))

   ;;; Stateful containers
   (:module "stateful"
    :depends-on ("interface")
    :components
    ((:file "package")
     (:file "container" :depends-on ("package"))
     (:file "binary-heap" :depends-on ("container"))
     (:file "binomial-heap" :depends-on ("container"))
     (:file "fifo" :depends-on ("container"))
     (:file "dllist" :depends-on ("container"))
     (:file "sorting" :depends-on ("binary-heap" "binomial-heap"))))))

(defmethod perform ((op test-op) (system (eql (find-system :fare-utils))))
  (asdf:load-system :fare-utils-test)
  (funcall (read-from-string "fare-utils-test:test-suite")))
