;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Plumbing to Define Interfaces

#+xcvb (module (:depends-on ("interface/package")))

(in-package :interface)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass interface-class (standard-class)
    ((generics :initform (make-hash-table :test 'eql) :accessor interface-generics)))

  (defmethod closer-mop:validate-superclass
      ((class interface-class) (super-class standard-class))
    t)

  (defun register-interface-generic
      (class name &rest keys &key in out)
    (declare (ignore in out))
    (setf (gethash name (interface-generics (find-class class))) keys)
    (values))

  (defun interface-direct-generics (interface)
    (loop :for name :being :the :hash-key :of (interface-generics interface)
      :collect name))

  (defgeneric all-superclasses (classes)
    (:method ((symbol symbol))
      (all-superclasses (find-class symbol)))
    (:method ((class class))
      (closer-mop:class-precedence-list class))
    (:method ((classes cons))
      (remove-duplicates
       (mapcan #'closer-mop:class-precedence-list classes)
       :from-end t)))

  (defun all-interface-generics (interfaces)
    (remove-duplicates
     (loop :for class :in (all-superclasses interfaces)
       :when (typep class 'interface-class)
       :append (interface-direct-generics class))))

  (defun search-gf-options (classes gf)
    (loop :for class :in classes
      :when (typep class 'interface-class) :do
      (multiple-value-bind (options foundp)
          (gethash gf (interface-generics class))
        (when foundp
          (return (values options t))))
      :finally (return (values nil nil))))

  (defun interface-gf-options (interface gf)
    (search-gf-options (all-superclasses interface) gf)))

(defmacro define-interface (interface super-interfaces slots &rest options)
  (let ((class-options
         (remove '(:default-initargs :documentation :metaclass)
                 options :key 'car :test-not #'(lambda (x y) (member y x))))
        (metaclass (find :metaclass options :key 'car))
        (generics (remove :generic options :key 'car :test-not 'eq))
        (methods (remove :method options :key 'car :test-not 'eq))
        (singleton (find :singleton options :key 'car))
        (parametric (find :parametric options :key 'car)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,interface ,super-interfaces ,slots
           ,@(unless metaclass `((:metaclass interface-class)))
           ,@class-options))
       ,@(when (or parametric singleton)
           (destructuring-bind (formals &body body)
               (or (cdr parametric)
                   '(() (make-interface)))
             `((define-memo-function
                   (,interface
                    :normalization
                    #'(lambda (make-interface &rest arguments)
                        (flet ((make-interface (&rest arguments)
                                 (apply make-interface arguments)))
                          (apply #'(lambda ,formals
                                     (block ,interface
                                       ,@body))
                                 arguments))))
                   (&rest arguments)
                 (apply 'make-instance ',interface arguments)))))
       ,@(when singleton `((defvar ,interface (,interface))))
       ,@(loop :for (nil . generic) :in generics :append
           (destructuring-bind (name &optional interface-options lambda-list
                                     &rest generic-options)
               generic
             `(,@(when lambda-list `((defgeneric ,name ,lambda-list ,@generic-options)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (apply 'register-interface-generic
                          ',interface ',name ',interface-options)))))
       ,@(when methods
           (with-gensyms (ivar)
             `((define-interface-methods (,ivar ,interface) ,@methods))))
       ',interface)))

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
         (symbol (all-interface-generics spec)))))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lambda-list-mimicker (lambda-list)
    (nest
     (multiple-value-bind (required optionals rest keywords allow-other-keys aux)
         (alexandria:parse-ordinary-lambda-list lambda-list)
       (declare (ignore aux)))
     (let* ((keyp (member '&key lambda-list)))
       (when (and keyp (not rest))
         (setf rest (gensym "KEYS")))
       (values
        ;; mimic-lambda-list
        (append required
                (when optionals (cons '&optional optionals))
                (when rest (list '&rest rest))
                (when keywords (cons '&key keywords))
                (when allow-other-keys '(&allow-other-keys)))
        ;; mimic-ignorables
        (remove-if #'null
                   (append
                    (mapcar #'caddr optionals)
                    (mapcar #'cadar keywords)
                    (mapcar #'caddr keywords)))
        ;; mimic-invoker
        (if rest 'apply 'funcall)
        ;; mimic-arguments
        (append required (mapcar #'car optionals) (when rest (list rest))))))))

(defmacro define-interface-methods ((interface-var interface-class) &body body)
  `(macrolet ((:method (name &rest rest)
                (finalize-inheritance (find-class ',interface-class))
                (if (length=n-p rest 1)
                    ;; One-argument: simply map a method to an interface-less function
                    (let* ((lambda-list (closer-mop:generic-function-lambda-list name)))
                      (multiple-value-bind (mimic-lambda-list mimic-ignorables
                                                              mimic-invoker mimic-arguments)
                          (lambda-list-mimicker lambda-list)
                        (let ((i-var (first mimic-lambda-list)))
                          `(defmethod ,name ((,i-var ,',interface-class) ,@(rest mimic-lambda-list))
                             (declare (ignorable ,i-var ,@mimic-ignorables))
                             (,mimic-invoker ,@rest ,@(rest mimic-arguments))))))
                    ;; More than one argument: a method that uses current interface
                    (destructuring-bind (lambda-list &rest body) rest
                      (multiple-value-bind (remaining-forms declarations doc-string)
                          (alexandria:parse-body body :documentation t)
                        `(defmethod ,name ((,',interface-var ,',interface-class) ,@lambda-list)
                           ,@doc-string ,@declarations (declare (ignorable ,',interface-var))
                           (with-interface (,',interface-var ,',interface-class)
                             ,@remaining-forms)))))))
     ,@body))

(define-interface <interface> ()
  ()
  (:documentation "An interface, encapsulating an algorithm"))
