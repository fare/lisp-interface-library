;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Pure to Stateful by Boxification: actual interfaces

#+xcvb
(module (:depends-on ("transform/mutating" "pure/map-interface" "stateful/map-interface")))

(in-package :stateful)

(define-interface <mutating> (<interface>)
  ((pure-interface
    :reader pure-interface
    :initarg :pure-interface))
  (:parametric (interface)
    (make-interface :pure-interface interface)))

(define-mutating-interface <mutating-map> (<map>) (pure:<map>)
  ()
  (:method empty! (x)
     (set-box-value (empty (pure-interface <mutating-map>)) x))
  (:method join/list (list)
     (cond
       (list
        (let ((f (first list)))
          (set-box-value
           (pure:join/list (pure-interface <mutating-map>)
                           (mapcar #'box-value list))
           f)
          f))
       (t
        (empty <mutating-map>))))
  (:method divide/list (map)
     (let ((list
            (pure:divide/list
             (pure-interface <mutating-map>)
             (box-value map))))
       (and list
            (progn
              (set-box-value (first list) map)
              (cons map (mapcar #'box! (rest list)))))))
  (:parametric (interface)
    (make-interface :pure-interface interface)))

(defparameter <alist> (<mutating-map> pure:<alist>))
