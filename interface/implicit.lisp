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

(defmacro with-interface ((interface-sexp functions-spec &key prefix package) &body body)
  (with-gensyms (arguments)
    (let ((function-names (collect-function-names functions-spec)))
      `(flet ,(loop :for function-name :in function-names
                :for local-name = (make-local-name function-name :prefix prefix :package package)
                :collect
                `(,local-name (&rest ,arguments)
                              (apply ',function-name ,interface-sexp ,arguments)))
       (declare (inline ,@function-names))
       ,@body))))

(defmacro define-interface-specialized-functions (interface-sexp functions-spec &key prefix package)
  (with-gensyms (arguments)
    (let ((function-names (collect-function-names functions-spec)))
      `(progn
         ,(loop :for function-name :in function-names
            :for local-name = (make-local-name function-name :prefix prefix :package package)
            :do (assert (not (eq local-name function-name)))
            :collect
            `(defun ,local-name (&rest ,arguments)
               (apply ',function-name ,interface-sexp ,arguments)))
         (declaim (inline ,@function-names))))))

;;; TODO: a macro to mass-define methods each with with-interface-methods

(defmacro define-interface-methods ((interface-var interface-class) &body body)
  `(macrolet ((defmethod* (name lambda-list &body body)
               (multiple-value-bind (remaining-forms declarations doc-string)
                   (alexandria:parse-body body :documentation t)
                 `(defmethod ,name ((,',interface-var ,',interface-class) ,@lambda-list)
                    ,@doc-string ,@declarations
                    (with-interface (,',interface-var ,',interface-class)
                      ,@remaining-forms)))))
     ,@body))
