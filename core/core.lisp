;;;;; Plumbing to Define Interfaces

(uiop:define-package :lil/core/core
    (:use :closer-common-lisp :fare-memoization :closer-mop
          :lil/core/utility)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   ;; The main user interface
   #:interface-class #:interface-class-p #:<interface>
   #:define-interface #:with-interface #:make-interface
   ;; Advanced interface
   #:interface-generics #:interface-abstract-p
   #:register-interface-generic #:interface-direct-generics
   #:all-super-interfaces #:collect-all-super-interfaces
   #:all-interface-generics #:search-gf-options #:interface-gf-options
   #:un<>ate #:expand-interface-method> #:define-interface-generic>
   #:define-interface-generic #:collect-function-names
   #:define-interface-specialized-functions
   #:define-interface-method #:define-interface-methods))
(in-package :lil/core/core)

;; Definitions used by define-interface and its clients.

(defclass interface-class (standard-class)
     ((generics :initform (make-hash-table :test 'eql) :accessor interface-generics)
      ;;(%direct-super-interfaces :accessor %direct-super-interfaces)
      (%all-super-interfaces :accessor %all-super-interfaces)
      (abstractp :initform nil :reader interface-abstract-p :type boolean)))

(defun interface-class-p (class)
  (typep class 'interface-class))

(defmethod closer-mop:validate-superclass
    ((class interface-class) (super-class standard-class))
  (or (eq (class-name class) '<interface>)
      (interface-class-p super-class)
      (member super-class (class-direct-superclasses (find-class '<interface>)))))

(defgeneric direct-super-interfaces (interface)
  (:method ((i interface-class))
    (remove-if-not 'interface-class-p (class-direct-superclasses i))))

(defun normalize-gf-io (lambda-list values in out)
  (let ((in (ensure-list in))
        (out (ensure-list out))
        (maxin (number-of-required-arguments lambda-list))
        (maxout (number-of-required-arguments values))
        (lin (length in))
        (lout (length out)))
    (assert (<= 1 maxin))
    (cond
      ((< lin lout) (appendf in (make-list (- lout lin))))
      ((< lout lin) (appendf out (make-list (- lin lout)))))
    (loop :for i :in in :for o :in out
          :collect
          (let ((i (etypecase i
                     (null (assert (integerp o)) i)
                     (integer (assert (< 0 i maxin)) i)
                     (symbol (or (position i lambda-list :end maxin)
                                 (error "~S not found in required arguments of lambda-list ~S" i lambda-list)))))
                (o (etypecase o
                     (boolean o)
                     (integer (assert (< -1 o maxout)) o)
                     (symbol (or (position o values :end maxout)
                                 (error "~S not found in required arguments of values ~S" i values))))))
            (list i o)))))

(defun register-interface-generic
    (class name &rest keys &key lambda-list values in out)
  (setf (gethash name (interface-generics (find-class class)))
        (list* :effects (normalize-gf-io lambda-list values in out) keys))
  (values))

(defun interface-direct-generics (interface)
  (loop :for name :being :the :hash-key :of (interface-generics interface)
        :collect name))

(defgeneric all-super-interfaces (interfaces)
  (:method ((symbol symbol))
    (when symbol
      (all-super-interfaces (find-class symbol))))
  (:method ((interface interface-class))
    (cond
      ((slot-boundp interface '%all-super-interfaces)
       (%all-super-interfaces interface))
      (t
       (let ((asi (with-unique-collector (c)
                    (collect-all-super-interfaces interface c))))
         (setf (%all-super-interfaces interface) asi)
         asi))))
  (:method ((interfaces cons))
    (with-unique-collector (c)
      (collect-all-super-interfaces interfaces c))))

(defgeneric collect-all-super-interfaces (interfaces collector)
  (:method ((symbol symbol) collector)
    (when symbol
      (collect-all-super-interfaces (find-class symbol) collector)))
  (:method ((interface interface-class) collector)
    (cond
      ((slot-boundp interface '%all-super-interfaces)
       (map () collector (%all-super-interfaces interface)))
      (t
       ;; (finalize-inheritance class)
       (funcall collector interface)
       (collect-all-super-interfaces (direct-super-interfaces interface) collector))))
  (:method ((interfaces cons) collector)
    (loop :for interface :in interfaces
          :do (collect-all-super-interfaces interface collector))))

(defun all-interface-generics (interfaces)
  (remove-duplicates
   (loop :for class :in (all-super-interfaces interfaces)
         :when (typep class 'interface-class)
         :append (interface-direct-generics class))))

(defun search-gf-options (classes gf)
  (loop :for class :in classes
        :when (typep class 'interface-class)
        :do (multiple-value-bind (options foundp)
                (gethash gf (interface-generics class))
              (when foundp
                (return (values options t))))
        :finally (return (values nil nil))))

(defun interface-gf-options (interface gf)
  (search-gf-options (all-super-interfaces interface) gf))

(defun un<>ate (string)
  (let ((string (string string)))
    (if (string-enclosed-p "<" string ">")
        (subseq string 1 (1- (length string)))
        string)))

(defun expand-interface-method> (interface gf x &rest y)
  (multiple-value-bind (interface-var interface-class)
      (etypecase interface
        (cons (values (first interface) (second interface)))
        (symbol (values interface interface)))
    (if (null y)
        ;; One-argument: simply map a method to an interface-less function
        (nest
         (let ((lambda-list (closer-mop:generic-function-lambda-list gf))
               (function x)))
         (multiple-value-bind (mimic-lambda-list
                               mimic-ignorables
                               mimic-invoker mimic-arguments #|mappings|#)
             (lambda-list-mimicker lambda-list))
         (let ((i-var (first mimic-lambda-list))))
         `(((,i-var ,interface-class) ,@(rest mimic-lambda-list))
           (declare (ignorable ,i-var ,@mimic-ignorables))
           (,mimic-invoker ,function ,@(rest mimic-arguments))))
        ;; More than one argument: a method that uses current interface
        (nest
         (multiple-value-bind (combination* spec)
             (etypecase x
               (list (values () (cons x y)))
               (symbol (values (list x) y))))
         (destructuring-bind (lambda-list &rest body) spec)
         (multiple-value-bind (remaining-forms declarations doc-string)
             (alexandria:parse-body body :documentation t))
         `(,@combination* ((,interface-var ,interface-class) ,@lambda-list)
                          ,@(when doc-string (list doc-string)) ,@declarations (declare (ignorable ,interface-var))
                          (with-interface (,interface-var ,interface-class)
                            ,@remaining-forms))))))

(defmacro define-interface-generic> (time interface name lambda-list &rest options)
  `(%define-interface-generic ,time ,interface ,interface ,name ,lambda-list ,@options))

(defmacro define-interface-generic (time interface name lambda-list &rest options)
  `(%define-interface-generic ,time nil ,interface ,name ,lambda-list ,@options))

(defmacro %define-interface-generic (time interface-argument interface name lambda-list &rest options)
  (let ((full-lambda-list `(,@(when interface-argument `(,interface-argument)) ,@lambda-list)))
    (ecase time
      (:register
       (let ((in (find-unique-clos-option :in options))
	     (out (find-unique-clos-option :out options))
	     (values (find-unique-clos-option :values options)))
	 `(eval-when (:compile-toplevel :load-toplevel :execute)
	    (register-interface-generic
	     ',interface ',name
	     :lambda-list ',full-lambda-list
	     ,@(when in `(:in ',(cdr in)))
	     ,@(when out `(:out ',(cdr out)))
	     ,@(when values `(:values ',(cdr values)))))))
      (:define
       (let ((generic-options
	       (remove-keyed-clos-options
		`(,@(when interface-argument '(:method>)) :in :out :values) options))
	     (methods> (when interface-argument (find-multiple-clos-options :method> options))))
	 `(defgeneric ,name ,full-lambda-list
	    ,@generic-options
	    ,@(loop :for (() . spec) :in methods> :collect
		    `(:method ,@(apply 'expand-interface-method>
				 (list interface-argument interface) name spec)))))))))

(defmacro define-interface (interface super-interfaces slots &rest options)
  (let ((class-options
         ;;(keep-keyed-clos-options '(:default-initargs :documentation :metaclass) options)
         (remove-keyed-clos-options
          '(:generic :generic> :method :method> :singleton :parametric :abstract :metaclass) options))
        (metaclass (find-unique-clos-option/1 :metaclass options 'interface-class))
        (gfs (find-multiple-clos-options :generic options))
        (gfs> (find-multiple-clos-options :generic> options))
        (methods (find-multiple-clos-options :method options))
        (methods> (find-multiple-clos-options :method> options))
        (parametric (find-unique-clos-option :parametric options))
        (singleton (find-unique-clos-option/0 :singleton options))
        (abstract (find-unique-clos-option/0 :abstract options)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,interface ,super-interfaces ,slots
           (:metaclass ,metaclass)
           ,@class-options))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((interface-class (find-class ',interface)))
	   (finalize-inheritance interface-class)
	   ,@(when abstract
	       '((setf (slot-value interface-class 'abstractp) t)))))
       ,@(when (or parametric singleton)
	   (assert (not abstract))
           (destructuring-bind (formals &body body)
               (or (cdr parametric)
                   '(() (make-interface)))
             `((eval-when (:compile-toplevel :load-toplevel :execute)
		 (define-memo-function
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
                   (apply 'make-instance ',interface arguments))))))
       ,@(when singleton `((eval-when (:compile-toplevel :load-toplevel :execute)
			     (defvar ,interface (,interface)))))
       ,@(loop :for (() . gf) :in gfs :collect
               `(define-interface-generic :register ,interface ,@gf))
       ,@(loop :for (() . gf>) :in gfs> :collect
               `(define-interface-generic> :register ,interface ,@gf>))
       ,@(loop :for (() . gf) :in gfs :collect
               `(define-interface-generic :define ,interface ,@gf))
       ,@(loop :for (() . gf>) :in gfs> :collect
               `(define-interface-generic> :define ,interface ,@gf>))
       ,@(when methods>
           `((define-interface-methods (,interface ,interface) ,@methods>)))
       ,@(mapcar #'(lambda (x) `(defmethod ,@(rest x))) methods)
       ',interface)))

(defun collect-function-names (functions-spec)
  (remove-duplicates
   (loop :for spec :in (alexandria:ensure-list functions-spec)
         :append
         (etypecase spec
           (list spec)
           (symbol (all-interface-generics spec))))))

(defmacro with-interface ((interface-sexp functions-spec &key prefix package) &body body)
  (with-gensyms (arguments)
    (let* ((function-names (collect-function-names functions-spec))
	   (local-names (loop :for function-name :in function-names :collect
			      (make-local-name function-name :prefix prefix :package package))))
      `(flet ,(loop :for function-name :in function-names
                :for local-name :in local-names
                :collect
                `(,local-name (&rest ,arguments)
                              (apply ',function-name ,interface-sexp ,arguments)))
         (declare (ignorable ,@(mapcar #'(lambda (x) `#',x) local-names))
		  (inline ,@local-names))
	 ,@body))))

(defmacro define-interface-specialized-functions (interface-sexp functions-spec &key prefix package)
  (with-gensyms (arguments)
    (let ((function-names (collect-function-names functions-spec)))
      `(progn
         ,@(loop :for function-name :in function-names
             :for local-name = (make-local-name function-name :prefix prefix :package package)
             :do (assert (not (eq local-name function-name)))
             :collect
             `(defun ,local-name (&rest ,arguments)
                (apply ',function-name ,interface-sexp ,arguments)))
         (declaim (inline ,@function-names))))))

(defmacro define-interface-method (interface gf &rest rest)
  `(defmethod ,gf ,@(apply 'expand-interface-method> interface gf rest)))

(defmacro define-interface-methods (interface &body body)
  `(macrolet ((:method> (gf &rest rest)
                `(define-interface-method ,',interface ,gf ,@rest)))
     ,@body))

