
(defpackage :interface/monad/test/monad/monads
  (:use :cl)
  (:import-from :interface/monad/test/monad)
  (:import-from :interface/monad/test/monad/identity)
  (:import-from :interface/monad/test/monad/maybe)
  (:import-from :interface/monad/test/monad/list)
  (:import-from :interface/monad/test/monad/state)
  (:import-from :interface/monad/test/monad/continuation)
  (:import-from :interface/monad/test/monad/transformer)
  (:import-from :interface/monad/test/monad/transformer/maybe)
  (:export #:test-monads))
(in-package :interface/monad/test/monad/monads)

(defparameter *standard-monads* 
  (list interface/monad/identity:<identity>
                      interface/monad/maybe:<maybe>
                      interface/monad/list:<list>
                      interface/monad/state:<state>
                      interface/monad/continuation:<continuation>))

(macrolet ((transformers (interface &optional prefix)
             (let ((name
                    (intern
                     (string-upcase
                      (concatenate
                       'string
                       "*"
                       (or prefix "")
                       (and prefix "-")
                       "transformer-standard-monads*")))))
               `(defparameter ,name
                  (list* ,interface
                         (mapcar (function ,interface)
                                 *standard-monads*))))))
  (transformers interface/monad/transformer:<transformer>)
  (transformers interface/monad/transformer/maybe:<maybe-transformer>
                "maybe")
  (transformers interface/monad/transformer/list:<list-transformer>
                "list"))
(defun test-monads
    (&optional (monads (append *standard-monads* 
                               *transformer-standard-monads*
                               *maybe-transformer-standard-monads*
                               *list-transformer-standard-monads*)))
  (loop :for m :in monads
     :collect (prog1 (interface:check-invariant interface/monad:<monad> m)
                (interface:check-invariant m m))))
