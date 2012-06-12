#+xcvb (module ())
(defpackage :fare-utils-test
  (:use :fare-utils
        :interface :eq :order :pure
        :reader-interception
        :cl :hu.dwim.stefil)
  (:export #:test-suite))

(in-package :fare-utils-test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing fare-utils"))
