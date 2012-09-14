;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; From Stateful to Pure by Linearization: actual interfaces

#+xcvb
(module (:depends-on ("transform/linearize" "pure/map-interface" "stateful/map-interface")))

(in-package :pure)

(define-interface <linear> (<interface>) () (:abstract))

(define-interface <linearized> (<linear>)
  ((stateful-interface
    :reader stateful-interface
    :initarg :stateful-interface)
   #|(box-interface
    :reader box-interface
    :initarg :box-interface :initform <one-use-value-box>)|#)
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))

(define-linearized-interface <linearized-map> (<map>) (stateful:<map>)
  ()
  (:method join/list (list)
     (cond
       (list
        (interface::one-use-box
         (stateful:join/list (stateful-interface <linearized-map>)
                             (mapcar 'interface::box-ref list))))
       (t
        (empty <linearized-map>))))
  (:method divide/list (map)
     (let ((list
            (stateful:divide/list
             (stateful-interface <linearized-map>)
             (interface::box-ref map))))
       (and list
            (mapcar 'interface::one-use-box list))))
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))
