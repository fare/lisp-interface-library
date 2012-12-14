
(defpackage :interface/monad/test/monad/monads
  (:use :cl)
  (:import-from :interface/monad/test/monad)
  (:import-from :interface/monad/test/monad/identity)
  (:import-from :interface/monad/test/monad/maybe)
  (:export #:test-monads))
(in-package :interface/monad/test/monad/monads)

(defun test-monads
    (&optional (monads
                (list interface/monad/identity:<identity>
                      interface/monad/maybe:<maybe>)))
  (loop :for m :in monads
     :collect (list (interface:check-invariant interface/monad:<monad> m)
                    (interface:check-invariant m m))))
