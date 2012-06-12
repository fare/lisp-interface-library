;;; updatef: a pure alternative to setf.
;;; generic macro to update places in an extensible way

(in-package :pure)

#|
An updatef expansion is an ordered collection of five objects:
  TEMP-VARS
    a list of symbols naming temporary variables to be bound sequentially,
    as if by let*, to values resulting from value forms.
  TEMP-VALS
    a list of forms (typically, subforms of the place) which when evaluated
    yield the values to which the corresponding temporary variables
    should be bound.
  BIND-VARS
    a list of symbols naming temporary store variables which are to hold
    the new values that will be assigned to the place in the updated state
  BINDER-FORM
    a form which can reference both the temporary and the store variables, and
    which returns an updated state in which the place has been assigned
    the updated values, which is the correct value for updatef to return.
  READER-FORM
    a form which can reference the temporary variables, and which returns
    the former value of the place in the state before the update.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)

(defgeneric updatef-expansion (expander &key op args place environment))

(defun get-updatef-expansion (place &optional environment)
  "pure analogue to (GET-SETF-EXPANSION PLACE ENVIRONMENT)"
  (check-type place cons)
  (destructuring-bind (op &rest args) place
    (check-type op symbol)
    (let ((expansion (get op 'updatef-expansion)))
      (unless expansion
        (error "No updatef expansion for ~S" op))
      (updatef-expansion expansion :op op :args args :place place :environment environment))))

(defmacro %define-updatef-expansion (access-fn value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',access-fn 'updatef-expansion) ,value)
     ',access-fn))

(defclass updatef-expander ()
  ((expander :reader updatef-expander :initarg :expander)))

(defmethod updatef-expansion ((u updatef-expander) &key op args place environment)
  (declare (ignore op))
  (apply (updatef-expander u) environment place args))

(defmacro define-updatef-expander (access-fn lambda-list &body body)
  "pure analogue to (DEFINE-SETF-EXPANDER ACCESS-FN LAMBDA-LIST . BODY)"
  (check-type access-fn symbol)
  (with-gensyms (args)
    (multiple-value-bind (destructuring-lambda-list wholevar wholep envvar envp)
        (parse-macro-lambda-list lambda-list)
      `(%define-updatef-expansion
        ,access-fn
        (make-instance
         'updatef-expander :expander
         #'(lambda (,envvar ,wholevar &rest ,args)
             ,@(unless wholep `((declare (ignore ,wholevar))))
             ,@(unless envp `((declare (ignore ,envvar))))
             (destructuring-bind (,@destructuring-lambda-list) ,args
               ,@body)))))))

(defun get-updatef-expansion-tmpvars (environment args)
  (loop
    :for arg :in args :for tmpvar = (gensym "ARG")
    :when (constantp arg environment)
      :collect arg :into actual-args
    :else
      :collect tmpvar :into actual-args :and
      :collect tmpvar :into tmpvars :and
      :collect arg :into inits
    :finally (return (values tmpvars inits actual-args))))

(defun simple-updatef-expansion (environment op args updater updatef-fun-p)
  (check-type updater symbol)
  (multiple-value-bind (tmpvars inits actual-args)
      (get-updatef-expansion-tmpvars environment args)
    (let ((newvalvar (gensym "VAL")))
      (values tmpvars inits newvalvar
              (if updatef-fun-p
                  `(,updater ,newvalvar ,@actual-args)
                  `(,updater ,@actual-args ,newvalvar))
              `(,op ,@actual-args)))))

(defclass defupdatef-short-expander (updatef-expander) ())

(defmethod updatef-expansion ((u defupdatef-short-expander) &key op args place environment)
  (declare (ignore place))
  (simple-updatef-expansion environment op args (updatef-expander u) nil))

(defclass defupdatef-function-expander (updatef-expander) ())

(defmethod updatef-expansion ((u defupdatef-function-expander) &key op args place environment)
  (declare (ignore place))
  (simple-updatef-expansion environment op args (updatef-expander u) t))

(defclass defupdatef-long-expander (updatef-expander)
  ((n-bind-vars :initarg :n-bind-vars :reader n-bind-vars)))

(defmethod updatef-expansion ((u defupdatef-long-expander) &key op args place environment)
  (declare (ignore place))
  (multiple-value-bind (tmpvars inits actual-args)
      (get-updatef-expansion-tmpvars environment args)
    (let* ((n (n-bind-vars u))
           (bind-vars (loop :repeat n :collect (gensym "VAL"))))
      (assert (= n (length args)))
      (values tmpvars inits bind-vars
              (funcall (updatef-expander u) environment (append bind-vars actual-args))
              `(,op ,@actual-args)))))

(defmacro defupdatef (access-fn &rest more)
  "pure analogue to defsetf"
  (etypecase (car more)
    (symbol ; short form
     (destructuring-bind (update-fn &optional docstring) more
       (declare (ignore docstring))
       `(%define-updatef-expansion
         ,access-fn
         (make-instance 'defupdatef-short-expander :expander ',update-fn))))
    (list ; long form
     (destructuring-bind (defsetf-lambda-list bind-vars &body body) more
       (assert (every 'identifierp bind-vars))
       (multiple-value-bind (lambda-list environment envp)
           (parse-defsetf-lambda-list defsetf-lambda-list)
         `(%define-updatef-expansion
           ,access-fn
           (make-instance
            'defupdatef-long-expander :n-bind-vars (length bind-vars) :expander
            #'(lambda (,environment ,@bind-vars ,@lambda-list)
                ,@(unless envp `((declare (ignore ,environment))))
                ,@body))))))))

(defmacro define-updatef-function (access-fn lambda-list &body body)
  "pure analogue to `(DEFUN (SETF ,FUNCTION) ,LAMBDA-LIST ,@BODY)"
  (multiple-value-bind (body decls doc) (parse-body body :documentation t)
    (declare (ignore doc))
    `(%define-updatef-expansion
      ,access-fn
      (make-instance
       'defupdatef-function-expander :expander
       #'(lambda ,lambda-list
           ,decls
           (block ,access-fn ,@body))))))

(defun updatef-function (sym)
  (assert (symbolp sym))
  (let ((u (get sym 'updatef-expansion)))
    (typecase u
      (defupdatef-function-expander
       (updatef-expander u))
      (null
       (error "No updatef function for symbol ~S" sym))
      (defupdatef-short-expander
       (let ((i (updatef-expander u)))
         (if (and (fboundp i) (not (macro-function i)))
             #'(lambda (v &rest args)
                 (apply i (append args (list v))))
             (error "updatef inverse for ~S is not a function" sym))))
      (t
       (error "Updater for symbol ~S is not a function" sym)))))

(defmacro updatef (&rest uargs &environment env)
  "pure analogue to SETF"
  (let ((nargs (length uargs)))
    (cond
      ((= nargs 2)
       (let ((place (first uargs))
             (value-form (second uargs)))
         (when (atom place)
           (error "A variable is not a suitable place for UPDATEF"))
         (let* ((op (first place))
                (args (rest place))
                (expansion (get op 'updatef-expansion)))
           (typecase expansion
             (null
              `(call-updatef-function ',op ,value-form ,args))
             (defupdatef-short-expander
              `(,(updatef-expander expansion) ,args ,value-form))
             (defupdatef-function-expander
              `(funcall (load-time-value (updatef-function ',op)) ,value-form ,args))
             (updatef-expander
              (multiple-value-bind (dummies vals newval binder getter)
                  (updatef-expansion expansion :op op :args args :place place :environment env)
                (declare (ignore getter))
                `(let* (,@(mapcar #'list dummies vals))
                    (multiple-value-bind ,newval ,value-form
                      ,binder))))))))
      ((oddp nargs)
       (error "odd number of args to UPDATEF"))
      (t
       `(values (loop :for (place value) :on uargs :by #'cddr :collect
                  `(updatef ,place ,value)))))))
);eval-when
