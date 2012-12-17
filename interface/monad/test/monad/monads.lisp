
(defpackage :interface/monad/test/monad/monads
  (:use :cl)
  (:import-from :interface/monad/test/monad)
  (:import-from :interface/monad/test/monad/identity)
  (:import-from :interface/monad/test/monad/maybe)
  (:import-from :interface/monad/test/monad/list)
  (:import-from :interface/monad/test/monad/state)
  (:import-from :interface/monad/test/monad/continuation)
  (:import-from :interface/monad/test/monad/transformer)
  (:export #:test-monads))
(in-package :interface/monad/test/monad/monads)

(defparameter *standard-monads* 
  (list interface/monad/identity:<identity>
                      interface/monad/maybe:<maybe>
                      interface/monad/list:<list>
                      interface/monad/state:<state>
                      interface/monad/continuation:<continuation>))

(defparameter *transformer-standard-monads* 
  (list* interface/monad/transformer:<transformer>
         (mapcar #'interface/monad/transformer:<transformer>
                 *standard-monads*)))
  
(defun test-monads
    (&optional (monads (append *standard-monads* 
                               *transformer-standard-monads*)))
  (loop :for m :in monads
     :collect (list (interface:check-invariant interface/monad:<monad> m)
                    (interface:check-invariant m m))))
