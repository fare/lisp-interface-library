;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Plumbing to Define Interfaces

#+xcvb (module ())

;; TODO: move all of these to some utility library, e.g. — fare-utils?
(uiop:define-package :lil/interface/utility
  (:use :closer-common-lisp :fare-memoization :closer-mop)
  (:mix :fare-utils :uiop :alexandria)
  (:export
   #:memberp #:number-of-required-arguments #:with-unique-collector
   #:lambda-list-mimicker #:decompose-function-name #:make-local-name
   #:keep-keyed-clos-options #:remove-keyed-clos-options
   #:find-unique-clos-option #:find-unique-clos-option/0 #:find-unique-clos-option/1*
   #:find-unique-clos-option/1 #:find-multiple-clos-options
   #:boolean-integer))
(in-package :lil/interface/utility)

;; Definitions used by define-interface and its clients.
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun memberp (list &rest keys &key test test-not key)
    (declare (ignore test test-not key))
    #'(lambda (x) (apply 'member x list keys)))

  (defun number-of-required-arguments (lambda-list)
    (or (position-if (memberp '(&optional &rest &key &environment &aux)) lambda-list)
        (length lambda-list)))

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
    "Like UIOP:WHILE-COLLECTING, but doesn't collect duplicates."
    `(call-with-unique-collector
      #'(lambda (,collector) ,@body) :test ,test))

  (defun lambda-list-mimicker (lambda-list &optional gensym-all)
    (nest
     (multiple-value-bind (required optionals rest keys allow-other-keys aux)
         (parse-ordinary-lambda-list lambda-list)
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
           (mkeys (loop :for (kv #|def kp|#) :in keys
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
    (remove key options :key 'car :test-not 'eq)))

(defun boolean-integer (bool)
  (if bool 1 0))
