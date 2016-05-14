;;;;; Functional queues, implementation

(uiop:define-package :lil/pure/queue-implementation
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/pure/queue))
(in-package :lil/pure/queue-implementation)

;; Simple pure queue as a CONS cell (or NIL if empty)
;; the CAR of which is a list of entries to dequeue first,
;; the CDR of which is a list of entries to dequeue last in reverse order.
;; Enqueueing can happen either at the head or tail of the queue;
;; which is the default is further specialized below.
(define-interface <simple-queue> (<empty-is-nil> <queue>) ()
  (:method> check-invariant (q &key)
    (check-type q list)
    (assert (or (null q) (car q) (cdr q)))
    (assert (and (null (cdr (last (car q))))
                 (null (cdr (last (cdr q)))))))
  (:method> create (entries &key)
    (when entries (cons entries nil)))
  (:method> contents (q &key)
    (append (car q) (reverse (cdr q))))
  (:method> dequeue (q)
    (let ((head (car q))
          (tail (cdr q)))
      (cond
        (head
         (values
          (let ((h (cdr head))) (when (or h tail) (cons h tail)))
          (car head)
          t))
        (tail
         (let* ((entries (reverse tail))
                (f (first entries))
                (r (rest entries)))
           (values (when r (cons r nil)) f t)))
        (t
         (values nil nil nil)))))
  (:method> first-entry (q)
    (let ((head (car q))
          (tail (cdr q)))
      (cond
        (head (values (car head) t))
        (tail (values (car (last tail)) t))
        (t (values nil nil)))))
  (:method> enqueue-last (q x)
    (cons (car q) (cons x (cdr q))))
  (:method> enqueue-first (q x)
    (cons (cons x (car q)) (cdr q)))
  (:method> enqueue-many (q list)
    (if q
        (create list)
        (cons (car q) (revappend list (cdr q))))))

(define-interface <queue-enqueue-last> (<queue>) ()
  (:method> enqueue (q x) (enqueue-last q x)))
(define-interface <fifo-queue> (<queue-enqueue-last> <simple-queue>) ()
  (:singleton))

(define-interface <queue-enqueue-first> (<queue>) ()
  (:method> enqueue (q x) (enqueue-first q x)))
(define-interface <lifo-queue> (<queue-enqueue-first> <simple-queue>) ()
  (:singleton))

