;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Making Interfaces Implicit

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

(defun make-local-name (name &key prefix package)
  (intern (if prefix (strcat (string prefix) (string name)) (string name))
	  (or package *package*)))

(defmacro with-interface-methods ((interface methods &key prefix package) &body body)
  (with-gensyms (arguments)
    `(flet
	 ,(loop :for method :in methods
		:for method-name = method ;; do something better later
		:for local-name = (make-local-name method-name :prefix prefix :package package)
		:collect
		`(,local-name (&rest ,arguments)
		    (apply ',method-name ,interface ,arguments)))
       ,@body)))
