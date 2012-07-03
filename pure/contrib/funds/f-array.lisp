
(in-package :pure)

(defun make-f-array (size &key (initial-contents nil) (initial-element nil))
  "A functional array of the given size with the given initial contents."
  (let ((length (length initial-contents)))
   (labels ((f (start end)
	      (if (= start end) 
		  (make-binary-tree)
		  (let ((midpoint (floor (+ end start) 2)))
		    (make-instance 'binary-tree 
				   :key midpoint :value (if (< start length)
							 (elt initial-contents midpoint)
							 initial-element)
				   :left (f start midpoint)
				   :right (f (1+ midpoint) end))))))
     (f 0 size))))

(defun f-array-elt (array index)
  "The element at the given index of the given array."
  (tree-find array index :test #'=))

(defun f-array-replace (array index element)
  "An array similar to the given array except that index and element are
associated in the returned array."
  (tree-insert array index element :test #'=))

(defun f-array-size (array)
  "The number of elements in the given array."
  (labels ((f (tree amount)
	     (if (tree-empty-p tree)
		 amount
		 (f (bt-right tree) (1+ (bt-key tree))))))
    (f array 0)))

(defun f-array-count (item f-array &key (key #'identity) (test #'eql))
  "The number of elements in the given f-array that satisfy the test."
  (tree-count item f-array 
	      :key #'(lambda (tree)
		       (funcall key (bt-value tree))) 
	      :test test))

(defun f-array-count-if (pred f-array &key (key #'identity))
  "The number of elements in the given f-array that satisfy the test."
  (tree-count-if pred f-array 
		 :key #'(lambda (tree)
			  (funcall key (bt-value tree)))))

(defun map-f-array (function f-array)
  "A new f-array whose elements are the results of the application
of the given function to the elements of the given f-array."
  (map-tree #'(lambda (tree)
		(funcall function (bt-value tree)))
	    f-array))

(defun f-array-as-list (f-array)
  (mapcar #'cdr (tree-as-alist f-array)))
