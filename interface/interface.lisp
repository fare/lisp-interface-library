;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Plumbing to Define Interfaces

#+xcvb (module (:depends-on ("interface/package")))

(in-package :interface)

;; Definitions used by define-interface and its clients.
(eval-when (:compile-toplevel :load-toplevel :execute)

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

  (defun memberp (list &rest keys &key test test-not key)
    (declare (ignore test test-not key))
    #'(lambda (x) (apply 'member x list keys)))

  (defun number-of-required-arguments (lambda-list)
    (or (position-if (memberp '(&optional &rest &key &environment &aux)) lambda-list)
        (length lambda-list)))

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
      (loop :for i :in in :for o :in out :collect
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

  (defun call-with-unique-collector (fun &key test)
    (let ((already-seen (make-hash-table :test (or test 'eql)))
          (accumulator '()))
      (funcall fun
               #'(lambda (x) (unless (gethash x already-seen)
                               (setf (gethash x already-seen) t)
                               (push x accumulator)
                               t)))
      (nreverse accumulator)))

  (defmacro with-unique-collector ((collector &key test) &body body)
    `(call-with-unique-collector
      #'(lambda (,collector) ,@body) :test ,test))

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
      (loop :for interface :in interfaces :do
        (collect-all-super-interfaces interface collector))))

  (defun all-interface-generics (interfaces)
    (remove-duplicates
     (loop :for class :in (all-super-interfaces interfaces)
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
    (search-gf-options (all-super-interfaces interface) gf))

  (defun keep-keyed-clos-options (keys options)
    (remove-if-not (memberp keys) options :key 'car))

  (defun remove-keyed-clos-options (keys options)
    (remove-if (memberp keys) options :key 'car))

  (defun find-unique-clos-option (key options)
    (let* ((found (member key options :key 'car))
           (again (member key (rest found) :key 'car)))
      (when again (error "option ~S appears more than once in ~S" key options))
      (car found)))

  (defun find-unique-clos-option/0 (key options)
    (let ((option (find-unique-clos-option key options)))
      (when option
	(assert (length=n-p option 1))
	t)))

  (defun find-unique-clos-option/1* (key options)
    (let ((option (find-unique-clos-option key options)))
      (when option
	(assert (length=n-p option 2)))
      option))

  (defun find-unique-clos-option/1 (key options &optional default)
    (let ((option (find-unique-clos-option/1* key options)))
      (if option (second option) default)))

  (defun find-multiple-clos-options (key options)
    (remove key options :key 'car :test-not 'eq))

  (defun un<>ate (string)
    (let ((string (string string)))
      (if (string-enclosed-p "<" string ">")
          (subseq string 1 (1- (length string)))
          string)))

  (defun lambda-list-mimicker (lambda-list &optional gensym-all)
    (nest
     (multiple-value-bind (required optionals rest keys allow-other-keys aux)
         (alexandria:parse-ordinary-lambda-list lambda-list)
       (declare (ignore aux)))
     (let ((keyp (and (or keys (member '&key lambda-list)) t))
           (mappings ())))
     (labels ((g (&rest rest) (gensym (format nil "~{~A~}" rest)))
              (m (s) (if gensym-all (g s) s))
              (p (x y) (push (cons x y) mappings))))
     (let ((mrequired (loop :for rvar :in required
                        :for mrvar = (m rvar)
                        :do (p rvar mrvar)
                        :collect mrvar))
           (moptionals (loop :for (ovar #|defaults:|#() opvar) :in optionals
                         :for movar = (m ovar)
                         :for mopvar = (if opvar (m opvar) (g ovar :p))
                         :do (p ovar movar) (when opvar (p opvar mopvar))
                         :collect (list movar () mopvar)))
           (mrest (cond
                    (rest
                     (let ((mrest (m rest)))
                       (p rest mrest)
                       mrest))
                    (keyp
                     (g 'keys))))
           (mkeys (loop :for (kv def kp) :in keys
                    :for (kw kvar) = kv
                    :for mkvar = (m kvar)
                    :do (p kvar mkvar)
                    :collect `((,kw ,mkvar)))))
       (values
        ;; mimic-lambda-list
        (append mrequired
                (when moptionals (cons '&optional moptionals))
                (when mrest (list '&rest mrest))
                (when keyp (cons '&key mkeys))
                (when allow-other-keys '(&allow-other-keys)))
        ;; mimic-ignorables
        (mapcar 'cadar mkeys)
        ;; mimic-invoker
        (if (or optionals rest keyp) 'apply 'funcall)
        ;; mimic-arguments
        (append mrequired
                (reduce
                 #'(lambda (moptional acc)
                     (destructuring-bind (movar default mopvar) moptional
                       (declare (ignore default))
                       `(if ,mopvar (cons ,movar ,acc) '())))
                 moptionals
                 :initial-value (if (or rest keyp) (list mrest) '())
                 :from-end t))
        (reverse mappings)))))

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
	     ,@doc-string ,@declarations (declare (ignorable ,interface-var))
	     (with-interface (,interface-var ,interface-class)
	       ,@remaining-forms)))))))

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
           ,@class-options)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun decompose-function-name (name)
    (etypecase name
      (symbol
       (values name nil nil))
      (cons
       (assert (consp (cdr name)))
       (let ((kind (first name))
             (n2 (second name)))
         (ecase kind
           ((setf) (values n2 'setf nil))
           #+sbcl ((sb-pcl::slot-accessor) (values (third name) kind (cons n2 (cdddr name)))))))))
  (defun make-local-name (name &key prefix package)
    (multiple-value-bind (symbol kind) (decompose-function-name name)
      (flet ((local-symbol ()
               (intern (if prefix (strcat (string prefix) (string symbol)) (string symbol))
                       (case package
                         ((nil) *package*)
                         ((t) (symbol-package symbol))
                         (otherwise package)))))
        (ecase kind
          ((nil) (values (local-symbol)))
          ((setf) `(setf ,(local-symbol)))))))
  (defun collect-function-names (functions-spec)
    (remove-duplicates
     (loop :for spec :in (alexandria:ensure-list functions-spec)
       :append
       (etypecase spec
         (list spec)
         (symbol (all-interface-generics spec)))))))

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
         ,(loop :for function-name :in function-names
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-interface <interface> () ()
    (:abstract)
    (:documentation "An interface, encapsulating an algorithm")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod shared-initialize :before ((instance <interface>) slot-names &key &allow-other-keys)
    (when (interface-abstract-p (class-of instance))
      (error "Trying to instantiate abstract interface ~S" (type-of instance)))))
