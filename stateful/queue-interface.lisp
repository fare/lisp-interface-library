;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional queues

#+xcvb (module (:depends-on ("pure/collection")))

(in-package :stateful)

(define-interface <queue> (interface::<finite-collection> <empty!able> <fount> <sink>) ()
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

;;; Mixins implementing simple cases for a lot of the above functions
(define-interface <fifo-queue> (<queue>) () (:abstract))
(define-interface <lifo-queue> (<queue>) () (:abstract))
