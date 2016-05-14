;;;;; From Pure to Stateful by Boxification: map interfaces

(uiop:define-package :lil/transform/mutating-map
  (:use :closer-common-lisp
        :core
        :lil/interface/box
        :lil/transform/mutating)
  (:use-reexport :lil/stateful/map)
  (:import-from :lil/pure/map)
  (:import-from :lil/pure/alist)
  (:export
   #:<mutating-map> #:<alist>))
(in-package :lil/transform/mutating-map)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(define-mutating-interface <mutating-map> (<map>) (lil/pure/map:<map>)
  ()
  (:method> empty! (x)
     (set-box-value (empty (pure-interface <mutating-map>)) x))
  (:method> join/list (list)
     (cond
       (list
        (let ((f (first list)))
          (set-box-value
           (lil/pure/collection:join/list
            (pure-interface <mutating-map>) (mapcar #'box-value list))
           f)
          f))
       (t
        (empty <mutating-map>))))
  (:method> divide/list (map)
     (let ((list
            (lil/pure/collection:divide/list
             (pure-interface <mutating-map>)
             (box-value map))))
       (and list
            (progn
              (set-box-value (first list) map)
              (cons map (mapcar #'box! (rest list)))))))
  (:parametric (interface)
    (make-interface :pure-interface interface)))

(defparameter <alist> (<mutating-map> lil/pure/alist:<alist>))
