;;;;; All general interfaces + common core of pure and stateful data structures

(uiop:define-package :lil/core/all
  (:nicknames :lil/core)
  (:use :closer-common-lisp)
  (:mix :fare-utils :uiop :alexandria)
  (:use-reexport
   :lil/core/metaclass
   :lil/core/utility
   :lil/core/interface))
