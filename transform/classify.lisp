;;;;; From Interfaces to Classes: Macros

(uiop:define-package :lil/transform/classify
  (:use :closer-common-lisp
        :lil/interface/utility
        :lil/interface/definition
        :lil/interface/base
        :lil/interface/box)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:define-classified-interface-class #:define-classified-method
   #:<linear> #:<linearized> #:stateful-interface #:class-interface))
(in-package :lil/transform/classify)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

;;; TODO: handle gf's with or without explicit override

(defmacro define-classified-method
    (class interface class-gf interface-gf &key
     interface-argument
     (extract-interface (first (ensure-list interface-argument)))
     (interface-keyword :interface)
     (value-keyword :value)
     (wrap `(make-instance ',class))
     (unwrap `(box-ref))
     (genericp t))
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (nest
   (let* ((gf-options (interface-gf-options interface interface-gf))
          (lambda-list (getf gf-options :lambda-list))
          (results (getf gf-options :values))
          (effects (getf gf-options :effects)))
     (assert gf-options)
     (assert lambda-list))
   (when effects)
   (multiple-value-bind (class-lambda-list
                         class-ignorables
                         class-invoker class-arguments
                         class-mappings)
       (lambda-list-mimicker lambda-list)
     (declare (ignore class-invoker class-arguments class-mappings)))
   (multiple-value-bind (interface-lambda-list
                         interface-ignorables
                         interface-invoker interface-arguments
                         interface-mappings)
       (lambda-list-mimicker lambda-list t)
     (declare (ignore interface-ignorables interface-mappings)))
   (multiple-value-bind (class-required class-optionals
                         class-rest class-keys class-allow-other-keys class-aux)
         (alexandria:parse-ordinary-lambda-list class-lambda-list)
     (declare (ignore class-keys class-allow-other-keys class-aux)))
   (multiple-value-bind (interface-required interface-optionals
                         interface-rest interface-keys interface-allow-other-keys interface-aux)
         (alexandria:parse-ordinary-lambda-list interface-lambda-list)
     (declare (ignore interface-keys interface-allow-other-keys interface-aux)))
   (multiple-value-bind (interface-results-lambda-list
                         interface-results-ignorables
                         interface-results-invoker interface-results-arguments
                         interface-results-mappings)
       (lambda-list-mimicker results t)
     (declare (ignore interface-results-invoker interface-results-arguments
                      interface-results-mappings)))
   (multiple-value-bind (class-results-lambda-list
                         class-results-ignorables
                         class-results-invoker class-results-arguments
                         class-results-mappings)
       (lambda-list-mimicker results t)
     (declare (ignore class-results-ignorables class-results-mappings)))
   (multiple-value-bind (interface-results-required interface-results-optionals
                         interface-results-rest interface-results-keys
                         interface-results-allow-other-keys interface-results-aux)
         (alexandria:parse-ordinary-lambda-list interface-results-lambda-list)
     (declare (ignore interface-results-keys interface-results-allow-other-keys
                      interface-results-aux)))
   (multiple-value-bind (class-results-required class-results-optionals
                         class-results-rest class-results-keys
                         class-results-allow-other-keys class-results-aux)
         (alexandria:parse-ordinary-lambda-list class-results-lambda-list)
     (declare (ignore class-results-keys class-results-allow-other-keys
                      class-results-aux)))
   (let ((first-object-index (first (find-if #'integerp effects :key 'car)))))
   (multiple-value-bind (extra-arguments interface-expression)
       (if first-object-index
           (values nil `(class-interface ,(nth first-object-index class-required)))
           (values (when interface-argument `(,interface-argument)) extract-interface))
     (assert interface-expression))
   (let ((interface-var (first interface-required))
         (lin (length interface-required))
         (lcin (length class-required))
         (lout (length interface-results-required))
         (lcout (length class-results-required)))
     (assert (plusp lin))
     (assert (= lin lcin))
     (assert (= lout lcout))
     (assert (= (length interface-optionals) (length class-optionals)))
     (assert (eq (and interface-rest t) (and class-rest t))))
   (loop
     :for (in out) :in effects
     :when (integerp in)
       :collect (list in out) :into effective-inputs :end
     :when (integerp out)
       :collect (list out in) :into effective-outputs :end
     :finally)
   (return)
   (let* ((required-input-bindings
           (loop :for (in ()) :in effective-inputs
             :for siv = (nth in class-required)
             :for piv = (nth in interface-required)
             :collect `(,piv (,@unwrap ,siv))))
          (required-output-bindings
           (loop :for (out ()) :in effective-outputs
             :for eior = (nth out interface-results-required)
             :collect `(,(nth out class-results-required)
			(,@wrap
			 ,@(when interface-keyword
			     `(,interface-keyword ,interface-var))
			 ,@(when value-keyword `(,value-keyword))
			 ,eior))))
          (ineffective-class-inputs
           (loop :for i :from 1 :below lin
             :for v :in (rest class-required)
             :unless (find i effective-inputs :key 'first)
             :collect v))
          (ineffective-interface-inputs
           (loop :for i :from 1 :below lin
             :for v :in (rest interface-required)
             :unless (find i effective-inputs :key 'first)
             :collect v))
          (ineffective-class-outputs
           (loop :for i :below lout
             :for v :in class-results-required
             :unless (find i effective-outputs :key 'first)
             :collect v))
          (ineffective-interface-outputs
           (loop :for i :below lout
             :for v :in interface-results-required
             :unless (find i effective-outputs :key 'first)
             :collect v))
          (interface-argument-bindings
           (append
            `((,interface-var ,interface-expression))
            required-input-bindings
            (loop :for iii :in ineffective-interface-inputs
              :for ici :in ineffective-class-inputs
              :collect `(,iii ,ici))
            (loop :for (io () iop) :in interface-optionals
              :for (co () cop) :in class-optionals
              :append `((,io ,co) (,iop ,cop)))
            (when interface-rest
              `((,interface-rest ,class-rest)))))
          (class-results-bindings
           (append
            required-output-bindings
            (loop :for iio :in ineffective-interface-outputs
              :for ico :in ineffective-class-outputs
              :collect `(,ico ,iio))
            (loop :for (iro () irop) :in interface-results-optionals
              :for (cro () crop) :in class-results-optionals
              :append `((,cro ,iro) (,crop ,irop)))
            (when class-results-rest
              `((,class-results-rest ,interface-results-rest)))))))
   `(,(if genericp 'defmethod 'defun) ,class-gf
      (,@extra-arguments
       ,@(if genericp
             (loop :for x :in (rest class-lambda-list)
               :for i :from 1 :collect
               (if (find i effective-inputs :key 'first)
                   `(,x ,class)
                   x))
             (rest class-lambda-list)))
      (declare (ignore ,@class-ignorables))
      (let* (,@interface-argument-bindings)
        (multiple-value-bind (,@interface-results-lambda-list)
            (,interface-invoker ',interface-gf ,interface-var ,@(rest interface-arguments))
          #-clisp (declare (ignore ,@interface-results-ignorables))
          (let* (,@class-results-bindings)
            (,class-results-invoker #'values ,@class-results-arguments)))))))

(defmacro define-classified-interface-class
    (name superclasses interface &optional slots &rest options)
  (let* ((all-interfaces (all-super-interfaces interface))
         (interface-gfs (all-interface-generics all-interfaces))
         (overridden-gfs (append (find-multiple-clos-options :method options)
                                 (find-multiple-clos-options :method> options)))
         (overridden-gfs-hash
          (alexandria:alist-hash-table (mapcar 'cdr overridden-gfs)))
         (interface-argument (find-unique-clos-option/1* :interface-argument options))
         (extract-interface (find-unique-clos-option/1* :extract-interface options))
         (interface-keyword (find-unique-clos-option/1* :interface-keyword options))
         (value-keyword (find-unique-clos-option/1* :value-keyword options))
         (wrap (find-unique-clos-option/1* :wrap options))
         (unwrap (find-unique-clos-option/1* :unwrap options))
         (genericp (find-unique-clos-option/1* :genericp options))
         (package (find-unique-clos-option/1 :package options (symbol-package name)))
         (constructor-prefix (find-unique-clos-option/1 :constructor-prefix options))
         (constructor-suffix (find-unique-clos-option/1 :constructor-suffix options))
         (constructors-only (find-unique-clos-option/1 :constructors-only options))
         (class-options (remove-keyed-clos-options
                             '(:interface-argument :extract-interface
                               :interface-keyword :value-keyword
                               :wrap :unwrap :genericp :package
			       :constructor-prefix :constructor-suffix :constructors-only)
			     options)))
    `(progn
       (defclass ,name (,@superclasses #|stateful::<mutating>|#)
         ,slots
         ,@class-options)
       ,@(loop :for interface-gf :in interface-gfs
           :unless (gethash interface-gf overridden-gfs-hash) :append
           (let* ((effects (getf (search-gf-options all-interfaces interface-gf) :effects))
                  (constructorp (not (find-if #'integerp effects :key 'car))))
             ;; methods that have registered effects as expressible and expressed in our trivial language
             (when effects
               `(,@(when (or (and (not constructorp) (not constructors-only))
			     (and constructorp (or interface-argument extract-interface)))
                     (let ((class-gf (intern (symbol-name interface-gf) package)))
                       `((define-classified-method
                             ,name ,interface ,class-gf ,interface-gf
                           ,@interface-argument
                           ,@extract-interface
                           ,@interface-keyword
                           ,@value-keyword
                           ,@wrap
                           ,@unwrap
                           ,@genericp))))
                 ,@(when (and constructorp (or constructor-prefix constructor-suffix))
                     (let ((class-gf (format-symbol package "~@[~A~]~A~@[~A~]"
                                                    constructor-prefix (symbol-name interface-gf)
                                                    constructor-suffix)))
                       `((define-classified-method
                             ,name ,interface ,class-gf ,interface-gf
                           :extract-interface ,interface
                           ,@interface-keyword
                           ,@value-keyword
                           ,@wrap
                           ,@unwrap
                           :genericp nil)))))))))))
