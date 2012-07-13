#+xcvb (module ())
(defpackage :lisp-interface-library-test
  (:use :interface :eq :order :pure :asdf
        :reader-interception
        :cl :hu.dwim.stefil)
  (:export #:test-suite))

(in-package :lisp-interface-library-test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing lisp-interface-library"))
