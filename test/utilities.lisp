#+xcvb (module (:depends-on ("package")))

(in-package :lisp-interface-library-test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing lisp-interface-library"))

(defun sort-alist (alist) (sort (copy-seq alist) #'< :key #'car))
(defun shuffle-list (list)
  (mapcar #'cdr
          (sort (mapcar #'(lambda (x) (cons (random most-positive-fixnum) x)) list)
                #'< :key #'car)))
(defun make-alist (n &optional (formatter "~D"))
  (loop :for i :from 1 :to n :collect
    (cons i (format nil formatter i))))
(defun equal-alist (x y)
  (equal (sort-alist x) (sort-alist y)))
(defun alist-map* (i alist)
  (check-invariant i (alist-map i alist)))
(defun map-alist* (i map)
  (check-invariant i map)
  (map-alist i map))

(defparameter *alist-10-latin* (make-alist 10 "~@R"))
(defparameter *alist-100-decimal* (make-alist 100 "~D"))
(defparameter *alist-100-latin* (make-alist 100 "~@R"))
(defparameter *alist-100-english* (make-alist 100 "~R"))

(defparameter *al-1* (shuffle-list *alist-100-decimal*))
(defparameter *al-2* (remove-if-not #'evenp *alist-100-decimal* :key #'car))
(defparameter *al-3* (remove-if-not #'(lambda (x) (< (length x) 5)) *alist-100-latin* :key #'cdr))
(defparameter *al-5* (remove-duplicates (append *al-2* *al-3*) :key #'car :from-end t))

(defgeneric interface-test (<interface>))

(defvar *verbose* nil)

(defmacro X (&rest rest) `(when *verbose* (DBG ,@rest)))
