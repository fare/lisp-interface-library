;;;;; All general interfaces + common core of pure and stateful data structures

(uiop:define-package :lil/core/all
  (:nicknames :core)
  (:use :closer-common-lisp)
  (:mix :fare-utils :uiop :alexandria)
  (:use-reexport
   :lil/core/core
   :lil/core/utility
   :lil/core/interface))
