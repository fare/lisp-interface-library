;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Pure to Stateful and back

#+xcvb (module (:depends-on ("interface/interface" "interface/box")))

(in-package :interface)

;;; TODO: handle gf's with or without explicit override

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-matching-pure-method (name &key package methods in out superclasses)
    (let* ((pure-name (or (cadr (assoc name methods))
                          (find-symbol (string name) package)))
           (options (search-gf-options pure-name superclasses)))
      (NIY pure-name options in out))))

(defun %define-linearized-method
    (linearized-interface
     pure-method pure-lambda-list pure-values pure-effects
     stateful-method stateful-lambda-list stateful-values stateful-effects)
  (let ((i-var (first pure-lambda-list)))
  `(defmethod ,pure-method ((,i-var ,linearized-interface) ,@(rest pure-lambda-list))
     ,(NIY pure-values pure-effects
           stateful-method stateful-lambda-list stateful-values stateful-effects))))

(defmacro define-linearized-interface
    (names superclasses &rest options)
  (destructuring-bind (stateful-name
                       &optional (linearized-name (alexandria:symbolicate '#:linearized- stateful-name))
                       &key (package *package*) methods)
      (alexandria:ensure-list names)
    (let* ((stateful-name (first names))
           (generics (all-interface-generics stateful-name))
           (overridden-methods (remove :method options :key 'car :test-not 'eq))
           (overridden-methods-hash (alexandria:alist-hash-table overridden-methods :test 'eq))
           (all-superclasses (all-superclasses superclasses))
           (superclass-generics (all-interface-generics superclasses)))
      `(define-interface ,linearized-name (<one-use-box> ,@superclasses)
         ()
         ,@(loop :for generic :in generics
             :unless (gethash generic overridden-methods-hash) :do
             (destructuring-bind (&key in out) (search-gf-options all-superclasses generic)
               ;; methods that side-effect
               (when (and in out (eq t (car (alexandria:ensure-list out))))
                 (let ((pure-method (find-matching-pure-method
                                     generic :package package :in in :out out
                                     :superclasses all-superclasses
                                     :methods methods)))
                   (NIY pure-method superclass-generics)))))
       ,@options))))
