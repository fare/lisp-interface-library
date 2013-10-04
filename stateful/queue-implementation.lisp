;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Stateful queues, straightforward implementation

#+xcvb (module (:depends-on ("stateful/queue-interface")))

(in-package :stateful)

;; Simple stateful fifo queue as a CONS cell
;; the CDR of which is the list of entries, and
;; the CAR of which is the last cons cell in that list of entries.
(define-interface <simple-queue> (<queue>) ()
  (:method> check-invariant (q &key)
    (check-type q cons)
    (assert (eq (car q) (last q))))
  (:method> empty ()
    (let ((c (cons nil nil)))
      (setf (car c) c)
      c))
  (:method> empty-p (q)
    (null (cdr q)))
  (:method> empty! (q)
    (setf (car q) q (cdr q) nil))
  (:method> create (contents &key)
    (if contents
        (let ((list (copy-list contents)))
          (cons (last list) list))
        (empty)))
  (:method> contents (q &key)
    (copy-list (cdr q)))
  (:method> enqueue-last (q x)
    (let ((c (list x)))
      (setf (cdar q) c
            (car q) c)
      (values)))
  (:method> enqueue-first (q x)
    (cond
      ((null (cdr q)) (enqueue-last q x))
      (t (push x (cdr q)) (values))))
  (:method> dequeue (q)
    (if (null (cdr q))
        (values nil nil)
        (let ((x (pop (cdr q))))
          (when (null (cdr q)) (setf (car q) q))
          (values x t))))
  (:method> first-entry (q)
    (if (null (cdr q))
        (values nil nil)
        (values (cadr q) t)))
  (:method> enqueue-many (q list)
    (when list
      (let ((list (copy-list list)))
        (setf (cdar q) list
              (car q) (last list))))
    (values))
  (:method> dequeue-all (q)
    (prog1 (cdr q)
      (empty! q))))


(define-interface <queue-enqueue-last> (<queue>) ()
  (:method> enqueue (q x) (enqueue-last q x)))
(define-interface <fifo-queue> (<queue-enqueue-last> <simple-queue>) ()
  (:singleton))

(define-interface <queue-enqueue-first> (<queue>) ()
  (:method> enqueue (q x) (enqueue-first q x)))
(define-interface <lifo-queue> (<queue-enqueue-first> <simple-queue>) ()
  (:singleton))

