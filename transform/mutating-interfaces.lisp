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
     (interface::set-box-value (empty (pure-interface <mutating-map>)) x))
  (:method join/list (list)
     (cond
       (list
        (let ((f (first list)))
          (interface::set-box-value
           (pure:join/list (pure-interface <mutating-map>)
                           (mapcar 'interface::box-value list))
           f)
          f))
       (t
        (empty <mutating-map>))))
  (:method divide/list (map)
     (let ((list
            (pure:divide/list
             (pure-interface <mutating-map>)
             (interface::box-value map))))
       (and list
            (progn
              (interface::set-box-value (first list) map)
              (cons map (mapcar 'interface::box! (rest list)))))))
  (:parametric (interface)
    (make-interface :pure-interface interface)))
