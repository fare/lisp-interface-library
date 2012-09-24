;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Pure to Stateful: Macros

#+xcvb (module (:depends-on ("interface/box" "stateful/package")))

(in-package :interface)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

;;; TODO: handle gf's with or without explicit override

(defmacro define-mutating-method
    (mutating-interface stateful-interfaces pure-interfaces
     stateful-gf pure-gf &key
     pure-lambda-list pure-values pure-effects pure-gf-options
     stateful-lambda-list stateful-values stateful-effects stateful-gf-options)
  (nest
   (let* ((pure-gf-options
           (or pure-gf-options
               (interface-gf-options pure-interfaces pure-gf)))
          (stateful-gf-options
           (or stateful-gf-options
               (interface-gf-options stateful-interfaces stateful-gf)))
          (pure-gf* (symbol-function pure-gf))
          (stateful-gf* (symbol-function stateful-gf))
          (pure-lambda-list
           (or pure-lambda-list
               (getf pure-gf-options :lambda-list)
               (closer-mop:generic-function-lambda-list pure-gf*)))
          (stateful-lambda-list
           (or stateful-lambda-list
               (getf stateful-gf-options :lambda-list)
               (closer-mop:generic-function-lambda-list stateful-gf*)))
          (pure-values
           (or pure-values
               (getf pure-gf-options :values)))
          (stateful-values
           (or stateful-values
               (getf stateful-gf-options :values)))
          (pure-effects
           (or pure-effects
               (getf pure-gf-options :effects)))
          (stateful-effects
           (or stateful-effects
               (getf stateful-gf-options :effects)))))
   (multiple-value-bind (pure-mimic-lambda-list
                         pure-mimic-ignorables
                         pure-mimic-invoker pure-mimic-arguments
                         pure-mimic-mappings)
       (lambda-list-mimicker pure-lambda-list t)
     (declare (ignore pure-mimic-ignorables pure-mimic-mappings)))
   (multiple-value-bind (stateful-mimic-lambda-list
                         stateful-mimic-ignorables
                         stateful-mimic-invoker stateful-mimic-arguments
                         stateful-mimic-mappings)
       (lambda-list-mimicker stateful-lambda-list)
     (declare (ignore stateful-mimic-invoker stateful-mimic-arguments stateful-mimic-mappings)))
   (multiple-value-bind (pure-required pure-optionals
                         pure-rest pure-keys pure-allow-other-keys pure-aux)
         (alexandria:parse-ordinary-lambda-list pure-mimic-lambda-list)
     (declare (ignore pure-keys pure-allow-other-keys pure-aux)))
   (multiple-value-bind (stateful-required stateful-optionals
                         stateful-rest stateful-keys stateful-allow-other-keys stateful-aux)
         (alexandria:parse-ordinary-lambda-list stateful-mimic-lambda-list)
     (declare (ignore stateful-keys stateful-allow-other-keys stateful-aux)))
   (multiple-value-bind (pure-results-lambda-list
                         pure-results-ignorables
                         pure-results-invoker pure-results-arguments
                         pure-results-mappings)
       (lambda-list-mimicker pure-values t)
     (declare (ignore pure-results-invoker pure-results-arguments
                      pure-results-mappings)))
   (multiple-value-bind (stateful-results-lambda-list
                         stateful-results-ignorables
                         stateful-results-invoker stateful-results-arguments
                         stateful-results-mappings)
       (lambda-list-mimicker stateful-values t)
     (declare (ignore stateful-results-ignorables stateful-results-mappings)))
   (multiple-value-bind (pure-results-required pure-results-optionals
                         pure-results-rest pure-results-keys
                         pure-results-allow-other-keys pure-results-aux)
         (alexandria:parse-ordinary-lambda-list pure-results-lambda-list)
     (declare (ignore pure-results-keys pure-results-allow-other-keys
                      pure-results-aux)))
   (multiple-value-bind (stateful-results-required stateful-results-optionals
                         stateful-results-rest stateful-results-keys
                         stateful-results-allow-other-keys stateful-results-aux)
         (alexandria:parse-ordinary-lambda-list stateful-results-lambda-list)
     (declare (ignore stateful-results-keys stateful-results-allow-other-keys
                      stateful-results-aux)))
   (let ((pi-var (first pure-required))
         (si-var (first stateful-required))
         (lpin (length pure-required))
         (lsin (length stateful-required))
         (lpout (length pure-results-required))
         (lsout (length stateful-results-required)))
     (assert (plusp lpin))
     (assert (= lpin lsin))
     (assert (= (length pure-optionals) (length stateful-optionals)))
     (assert (eq (and pure-rest t) (and stateful-rest t))))
   (loop
     :with lepout = 0 :with lesout = 0
     :for (pin pout) :in pure-effects
     :for (sin sout) :in stateful-effects
     :do (assert (eq (integerp pin) (integerp sin)))
         (assert (eq (null pin) (null sin))) ;; new is new
         (check-type pout (or integer null)) ;; pure is pure
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
             :for siv = (nth esi stateful-required)
             :for piv = (nth epi pure-required)
             :collect `(,piv (box-value ,siv))))
          (required-output-bindings
           (loop :for (eso esi epo ()) :in effective-outputs
             :when (integerp eso)
             :collect `(,(nth eso stateful-results-required)
                        ,(if (integerp esi)
                             (nth esi stateful-required)
                             `(box! ,(nth epo pure-results-required))))))
          (required-output-updates
           (loop :for (eso esi epo ()) :in effective-outputs
             :when (integerp esi)
             :collect `(set-box-value
                        ,(nth epo pure-results-required)
                        ,(nth esi stateful-required))))
          (ineffective-stateful-inputs
           (loop :for i :from 1 :below lsin
             :for v :in (rest stateful-required)
             :unless (find i effective-inputs :key 'first)
             :collect v))
          (ineffective-pure-inputs
           (loop :for i :from 1 :below lpin
             :for v :in (rest pure-required)
             :unless (find i effective-inputs :key 'third)
             :collect v))
          (ineffective-stateful-outputs
           (loop :for i :below lpout
             :for v :in stateful-results-required
             :unless (find i effective-outputs :key 'first)
             :collect v))
          (ineffective-pure-outputs
           (loop :for i :below lpout
             :for v :in pure-results-required
             :unless (find i effective-outputs :key 'third)
             :collect v))
          (pure-argument-bindings
           (append
            `((,pi-var (pure-interface ,si-var)))
            required-input-bindings
            (loop :for ipi :in ineffective-pure-inputs
              :for isi :in ineffective-stateful-inputs
              :collect `(,ipi ,isi))
            (loop :for (po () pop) :in pure-optionals
              :for (so () sop) :in stateful-optionals
              :append `((,po ,so) (,sop ,pop)))
            (when pure-rest
              `((,pure-rest ,stateful-rest)))))
          (stateful-results-bindings
           (append
            required-output-bindings
            (loop :for ipo :in ineffective-pure-outputs
              :for iso :in ineffective-stateful-outputs
              :collect `(,iso ,ipo))
            (loop :for (pro () prop) :in pure-results-optionals
              :for (sro () srop) :in stateful-results-optionals
              :append `((,sro ,pro) (,srop ,prop)))
            (when stateful-results-rest
              `((,stateful-results-rest ,pure-results-rest)))))))
   `(defmethod ,stateful-gf ((,si-var ,mutating-interface) ,@(rest stateful-mimic-lambda-list))
      (declare (ignore ,@stateful-mimic-ignorables))
      (let* (,@pure-argument-bindings)
        (multiple-value-bind (,@pure-results-lambda-list)
            (,pure-mimic-invoker ',pure-gf ,pi-var ,@(rest pure-mimic-arguments))
          (declare (ignore ,@pure-results-ignorables))
          (let* (,@stateful-results-bindings)
            ,@required-output-updates
            (,stateful-results-invoker #'values ,@stateful-results-arguments)))))))

(defmacro define-mutating-interface
    (name stateful-interfaces pure-interfaces &optional slots &rest options)
  (let* ((all-stateful-interfaces (all-super-interfaces stateful-interfaces))
         (stateful-gfs (all-interface-generics all-stateful-interfaces))
         (all-pure-interfaces (all-super-interfaces pure-interfaces))
         (pure-gfs (all-interface-generics all-pure-interfaces))
         (pure-gfs-hash
          (alexandria:alist-hash-table
           (mapcar #'(lambda (x) (cons (symbol-name x) x)) pure-gfs) :test 'equal))
         (overridden-gfs (find-multiple-clos-options :method options))
         (overridden-gfs-hash
          (alexandria:alist-hash-table
           (mapcar #'(lambda (x) (cons (second x) (nthcdr 2 x))) overridden-gfs) :test 'eql)))
    `(progn
       (define-interface ,name (stateful:<mutating> ,@stateful-interfaces)
         ,slots
         ,@options)
       ,@(loop :for stateful-gf :in stateful-gfs
           :unless (gethash stateful-gf overridden-gfs-hash) :append
           (nest
            (let ((stateful-effects (getf (search-gf-options all-stateful-interfaces stateful-gf) :effects))))
            ;; methods that have registered effects as expressible and expressed in our trivial language
            (when stateful-effects)
            (let ((pure-gf (gethash (symbol-name stateful-gf) pure-gfs-hash))))
            (when pure-gf)
            (let ((pure-effects (getf (search-gf-options all-pure-interfaces pure-gf) :effects)))
              (assert pure-effects))
            `((define-mutating-method ,name ,stateful-interfaces ,pure-interfaces
                                      ,stateful-gf ,pure-gf)))))))
