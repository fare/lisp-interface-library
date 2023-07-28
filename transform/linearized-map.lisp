;;;;; From Stateful to Pure by Linearization: map interfaces

(uiop:define-package :lil/transform/linearized-map
  (:use :closer-common-lisp
        :lil/core
        :lil/interface/box
        :lil/transform/linearize)
  (:use-reexport :lil/pure/map)
  (:import-from :lil/stateful/map)
  (:export
   #:<linearized-map>))
(in-package :lil/transform/linearized-map)

(define-linearized-interface <linearized-map> (<map>) (lil/stateful/map:<map>)
  ()
  (:method> join/list (list)
     (cond
       (list
        (one-use-value-box
         (lil/stateful/collection:join/list (stateful-interface <linearized-map>)
                                            (mapcar #'box-ref list))))
       (t
        (empty))))
  (:method> divide/list (map)
     (let ((list
            (lil/stateful/collection:divide/list
             (stateful-interface <linearized-map>)
             (box-ref map))))
       (and list
            (mapcar #'one-use-value-box list))))
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))
