;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Making Interfaces Implicit

#+xcvb (module (:depends-on ("interface/interface")))

(in-package :interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-local-name (name &key prefix package)
    (intern (if prefix (strcat (string prefix) (string name)) (string name))
            (or package *package*)))
  (defun collect-function-names (functions-spec)
    (remove-duplicates
     (loop :for spec :in (alexandria:ensure-list functions-spec)
       :append
       (etypecase spec
         (list spec)
         (symbol (interface-all-generics spec)))))))

(defmacro with-interface-methods ((interface functions-spec &key prefix package) &body body)
  (with-gensyms (arguments)
    (let ((function-names (collect-function-names functions-spec)))
      `(flet ,(loop :for function-name :in function-names
                :for local-name = (make-local-name function-name :prefix prefix :package package)
                :collect
                `(,local-name (&rest ,arguments)
                              (apply ',function-name ,interface ,arguments)))
       (declare (inline ,@function-names))
       ,@body))))

