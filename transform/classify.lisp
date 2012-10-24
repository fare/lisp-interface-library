;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Interfaces to Classes: Macros

#+xcvb (module (:depends-on ("interface/interface" "interface/box")))

(in-package :interface)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

;;; TODO: handle gf's with or without explicit override

(defclass object-box (box!)
  ((interface :reader class-interface)))

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
         (lpin (length interface-required))
         (lsin (length class-required))
         (lpout (length interface-results-required))
         (lsout (length class-results-required)))
     (assert (plusp lpin))
     (assert (= lpin lsin))
     (assert (= (length interface-optionals) (length class-optionals)))
     (assert (eq (and interface-rest t) (and class-rest t))))
   (loop
     :with lepout = 0 :with lesout = 0
     :for (pin pout) :in effects
     :for (sin sout) :in effects
     :do (assert (eq (integerp pin) (integerp sin)))
         (assert (eq (null pin) (null sin))) ;; new is new
     :when (integerp pin)
       :collect (list sin sout pin pout) :into effective-inputs :end
     :when (integerp pout)
       :collect (list sout sin pout pin) :into effective-outputs :end
     :when (integerp pout)
       :do (incf lepout) :end
     :when (integerp sout)
       :do (incf lesout) :end
     :finally
     (assert (= (- lpout lepout) (- lsout lesout))))
   (return)
   (let* ((required-input-bindings
           (loop :for (esi () epi ()) :in effective-inputs
             :for siv = (nth esi class-required)
             :for piv = (nth epi interface-required)
             :collect `(,piv (,@unwrap ,siv))))
          (required-output-bindings
           (loop :for (eso esi epo ()) :in effective-outputs
             :for epor = (nth epo interface-results-required)
             :when (integerp eso)
             :collect `(,(nth eso class-results-required)
                        ,(if (integerp esi)
                             (progn
                               (push epor interface-results-ignorables)
                               (nth esi class-required))
                             `(,@wrap
                               ,@(when interface-keyword
                                       `(,interface-keyword ,interface-var))
                               ,@(when value-keyword `(,value-keyword))
                               ,epor)))))
          (ineffective-class-inputs
           (loop :for i :from 1 :below lsin
             :for v :in (rest class-required)
             :unless (find i effective-inputs :key 'first)
             :collect v))
          (ineffective-interface-inputs
           (loop :for i :from 1 :below lpin
             :for v :in (rest interface-required)
             :unless (find i effective-inputs :key 'third)
             :collect v))
          (ineffective-class-outputs
           (loop :for i :below lpout
             :for v :in class-results-required
             :unless (find i effective-outputs :key 'first)
             :collect v))
          (ineffective-interface-outputs
           (loop :for i :below lpout
             :for v :in interface-results-required
             :unless (find i effective-outputs :key 'third)
             :collect v))
          (interface-argument-bindings
           (append
            `((,interface-var ,interface-expression))
            required-input-bindings
            (loop :for ipi :in ineffective-interface-inputs
              :for isi :in ineffective-class-inputs
              :collect `(,ipi ,isi))
            (loop :for (po () pop) :in interface-optionals
              :for (so () sop) :in class-optionals
              :append `((,po ,so) (,sop ,pop)))
            (when interface-rest
              `((,interface-rest ,class-rest)))))
          (class-results-bindings
           (append
            required-output-bindings
            (loop :for ipo :in ineffective-interface-outputs
              :for iso :in ineffective-class-outputs
              :collect `(,iso ,ipo))
            (loop :for (pro () prop) :in interface-results-optionals
              :for (sro () srop) :in class-results-optionals
              :append `((,sro ,pro) (,srop ,prop)))
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
          (declare (ignore ,@interface-results-ignorables))
          (let* (,@class-results-bindings)
            (,class-results-invoker #'values ,@class-results-arguments)))))))

(defmacro define-classified-interface-class
    (name superclasses interface &optional slots &rest options)
  (let* ((all-interfaces (all-super-interfaces interface))
         (interface-gfs (all-interface-generics all-interfaces))
         (overridden-gfs (append (find-multiple-clos-options :method options)
				 (find-multiple-clos-options :method> options)))
         (overridden-gfs-hash
          (alexandria:alist-hash-table
           (mapcar (lambda (x) (cons (second x) (nthcdr 2 x))) overridden-gfs) :test 'eql))
         (interface-argument (find-unique-clos-option :interface-argument options))
         (extract-interface (find-unique-clos-option :extract-interface options))
         (interface-keyword (find-unique-clos-option :interface-keyword options))
         (value-keyword (find-unique-clos-option :value-keyword options))
         (wrap (find-unique-clos-option :wrap options))
         (unwrap (find-unique-clos-option :unwrap options))
         (genericp (find-unique-clos-option :genericp options))
         (class-options (remove-keyed-clos-options
                             '(:interface-argument :extract-interface
                               :interface-keyword :value-keyword
                               :wrap :unwrap :genericp) options))
         (package (symbol-package name)))
    `(progn
       (defclass ,name (,@superclasses #|stateful::<mutating>|#)
         ,slots
         ,@class-options)
       ,@(loop :for interface-gf :in interface-gfs
           :unless (gethash interface-gf overridden-gfs-hash) :append
           (nest
            (let ((effects (getf (search-gf-options all-interfaces interface-gf) :effects))))
            ;; methods that have registered effects as expressible and expressed in our trivial language
            (when effects)
            (when (or interface-argument
                      extract-interface
                      (find-if #'integerp effects :key 'car)))
            (let ((class-gf (intern (symbol-name interface-gf) package))))
            `((define-classified-method
                ,name ,interface ,class-gf ,interface-gf
                ,@interface-argument
                ,@extract-interface
                ,@interface-keyword
                ,@value-keyword
                ,@wrap
                ,@unwrap
                ,@genericp)))))))
