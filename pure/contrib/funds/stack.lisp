
(in-package :pure)

(defun make-stack ()
  "An empty stack."
  nil)

(defun stack-push (stack item)
  "The stack that results when the given item is pushed onto the given stack."
  (cons item stack))

(defun stack-pop (stack) 
  "The stack that results when the top item is popped off the given stack."
  (cdr stack))

(defun stack-top (stack) 
  "The top item on the given stack."
  (car stack))

(defun stack-empty-p (stack)
  "Whether the given stack is empty."
  (null stack))

(defun stack-size (stack)
  "The number of items on this stack; note that this is an O(n) operation."
  (labels ((f (stack accum)
	     (if (stack-empty-p stack)
		 accum
		 (f (stack-pop stack) (1+ accum)))))
    (f stack 0)))

(defun map-stack (function stack)
  "A stack whose elements are those of the given stack when function is applied
to them."
  (mapcar function stack))

(defun stack-from-list (list)
  "This function is here in case the implementation of stack changes from what 
it is now, a list."
  list)

(defun stack-as-list (stack)
  "This function is here in case the implementation of stack changes from what 
it is now, a list."
  stack)

(defun stack-count (item stack &key (key #'identity) (test #'eql))
  (count item stack :key key :test test))

(defun stack-count-if (predicate stack &key (key #'identity))
  (count-if predicate stack :key key))
