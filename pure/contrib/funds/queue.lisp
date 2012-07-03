
(in-package :pure)

(defclass queue ()
  ((next-priority :initarg :next-priority
		  :initform 0
		  :reader queue-next-priority)
   (heap :initarg :heap
	 :initform (make-heap)
	 :reader queue-heap)))

(defun make-queue (&key (initial-contents nil))
  (reduce #'(lambda (q n)
	      (enqueue q n))
	  initial-contents
	  :initial-value (make-instance 'queue)))

(defun queue-first (q)
  "The value at the head of the given queue."
  (heap-first (queue-heap q)))

(defun enqueue (q item)
  "The queue that results when the given item is equeued on the given queue."
  (make-instance 'queue :next-priority (1+ (queue-next-priority q))
		 :heap (heap-insert (queue-heap q) item (queue-next-priority q))))

(defun dequeue (q)
  "The queue that results when the first item is removed from the given queue."
  (if (queue-empty-p q)
      q
      (make-instance 'queue  :next-priority (1- (queue-next-priority q))
		     :heap (heap-remove (queue-heap q)))))

(defun queue-size (q)
  "The number of items in the given queue."
  (tree-weight (queue-heap q)))

(defun queue-empty-p (q)
  "Whether the given queue does not contain any items."
  (tree-empty-p (queue-heap q)))

(defun map-queue (function q)
  "A queue containing items that are the result of applying function to
the items in the given queue."
  (make-instance 'queue :next-priority (queue-next-priority q)
		 :heap (map-tree #'(lambda (tree)
				  (funcall function (bt-value tree)))
			      (queue-heap q))))

(defun queue-as-list (q)
  "The elements in the given queue, returned as a list, in the order they
would be dequeued from the given queue."
  (mapcar #'cdr (sort (tree-as-alist (queue-heap q))
		       #'< :key #'car)))

(defun queue-count (item q &key (key #'identity) (test #'eql))
  "The number of elements in the given queue that satisfy the test."
  (tree-count item (queue-heap q) 
	      :key #'(lambda (tree)
		       (funcall key (bt-value tree)))
	      :test test))

(defun queue-count-if (predicate q &key (key #'identity))
  "The number of elements in the given queue that satisfy the test."
  (tree-count-if predicate (queue-heap q)
		 :key #'(lambda (tree)
			  (funcall key (bt-value tree)))))
