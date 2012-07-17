;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :lisp-interface-library
  :description "LIL: abstract interfaces and supporting concrete data structures"
  :long-description "LIL is a collection of mostly pure data structures in interface-passing-style.
See http://fare.livejournal.com/155094.html regarding interface-passing-style,
or our upcoming ILC'2012 article http://github.com/fare/lil-ilc2012/
"
  :depends-on (:interface :pure-datastructures :stateful-datastructures))

(defmethod perform ((op test-op) (system (eql (find-system :lisp-interface-library))))
  (asdf:load-system :lisp-interface-library-test)
  (funcall (read-from-string "lisp-interface-library-test:test-suite")))
