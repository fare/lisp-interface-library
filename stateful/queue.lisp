;;;;; Stateful queues
(uiop:define-package :lil/stateful/queue
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/stateful/iterator
   :lil/stateful/collection)
  (:export
   #:<queue> #:enqueue #:dequeue #:enqueue-first #:enqueue-last #:enqueue-many #:dequeue-all
   #:<simple-queue>
   #:<fifo-queue> #:<lifo-queue> #:<queue-enqueue-last> #:<queue-enqueue-first>))
(in-package :lil/stateful/queue)

(define-interface <queue> (lil/interface/collection:<finite-collection> <empty!able> <fount> <sink>) ()
  (:abstract)
  (:generic> enqueue (queue datum) (:in 1) (:values) (:out t)
   (:documentation "Enqueue a datum into a queue, whether it's first or last makes the queue LIFO or FIFO"))
  (:generic> dequeue (queue) (:in 1) (:values value foundp) (:out t)
   (:documentation "Dequeue a datum from a queue"))
  (:generic> enqueue-first (queue datum) (:in 1) (:values map) (:out t)
   (:documentation "Enqueue a datum where it will be dequeued first"))
  (:generic> enqueue-last (queue datum) (:in 1) (:values map) (:out t)
   (:documentation "Enqueue a datum where it will be dequeued last"))
  (:generic> enqueue-many (queue list) (:in 1) (:values map) (:out t)
   (:documentation "Enqueue a list of data in order they should be dequeued"))
  (:generic> dequeue-all (queue) (:in 1) (:values map) (:out t)
   (:documentation "Empty the queue, return the former contents")))

