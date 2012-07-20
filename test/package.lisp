#+xcvb (module ())
(defpackage :lisp-interface-library-test
  (:use :interface :eq :order :asdf
        :reader-interception :fare-utils
        :cl :hu.dwim.stefil)
  (:export
   #:test-suite
   #:sort-alist
   #:shuffle-list
   #:make-alist
   #:equal-alist
   #:interface-test
   #:alist-map*
   #:*verbose* #:x
   #:*alist-10-latin*
   #:*alist-100-decimal*
   #:*alist-100-latin*
   #:*alist-100-english*
   #:*al-1*
   #:*al-2*
   #:*al-3*
   #:*al-5*))

(defpackage :lisp-pure-datastructure-test
  (:use :pure :lisp-interface-library-test
        :interface :eq :order :asdf
        :reader-interception :fare-utils
        :cl :hu.dwim.stefil))

(defpackage :lisp-stateful-datastructure-test
  (:use :stateful :lisp-interface-library-test
        :interface :eq :order :asdf
        :reader-interception :fare-utils
        :cl :hu.dwim.stefil)
  (:import-from :pure #:<alist>))
