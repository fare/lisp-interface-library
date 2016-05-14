(defpackage :lil/interface/monad/test/monad/monads
  (:use :cl
        :core
        :lil/interface/base
        :lil/interface/monad
        :lil/interface/monad/identity
        :lil/interface/monad/maybe
        :lil/interface/monad/list
        :lil/interface/monad/state
        :lil/interface/monad/continuation
        :lil/interface/monad/transformer
        :lil/interface/monad/transformer/list
        :lil/interface/monad/transformer/maybe
        :lil/interface/monad/test/monad
        :lil/interface/monad/test/monad/identity
        :lil/interface/monad/test/monad/maybe
        :lil/interface/monad/test/monad/list
        :lil/interface/monad/test/monad/state
        :lil/interface/monad/test/monad/continuation
        :lil/interface/monad/test/monad/transformer
        :lil/interface/monad/test/monad/transformer/maybe)
  (:export #:test-monads))
(in-package :lil/interface/monad/test/monad/monads)

(defparameter *standard-monads* 
  (list <identity> <maybe> <list> <state> <continuation>))

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
  (transformers <transformer>)
  (transformers <maybe-transformer> "maybe")
  (transformers <list-transformer> "list"))

(defun test-monads
    (&optional (monads (append *standard-monads* 
                               *transformer-standard-monads*
                               *maybe-transformer-standard-monads*
                               *list-transformer-standard-monads*)))
  (loop :for m :in monads
     :collect (prog1 (check-invariant <monad> m)
                (check-invariant m m))))
