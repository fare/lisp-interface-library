#+xcvb (module (:depends-on ("package")))
(in-package :lisp-interface-library-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-functional-map
            :in test-suite
            :documentation "Testing pure functional maps"))

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

(defparameter *alist-10-latin* (make-alist 10 "~@R"))
(defparameter *alist-100-decimal* (make-alist 100 "~D"))
(defparameter *alist-100-latin* (make-alist 100 "~@R"))
(defparameter *alist-100-english* (make-alist 100 "~R"))

(defparameter *al-1* (shuffle-list *alist-100-decimal*))
(defparameter *al-2* (remove-if-not #'evenp *alist-100-decimal* :key #'car))
(defparameter *al-3* (remove-if-not #'(lambda (x) (< (length x) 5)) *alist-100-latin* :key #'cdr))
(defparameter *al-5* (remove-duplicates (append *al-2* *al-3*) :key #'car :from-end t))

(defun alist-from (i map)
  (convert <alist> i map))

(defun from-alist (i map)
  (check-invariant i (convert i <alist> map)))

(defgeneric interface-test (<interface>))

(defmethod interface-test ((i <map>))
  ;;; TODO: test each and every function in the API
  (is (null (alist-from i (empty i))))
  (is (empty-p i (from-alist i ())))
  (is (equal "12"
             (lookup
              i
              (from-alist
               i '((57 . "57") (10 . "10") (12 . "12")))
              12)))
  (loop :for (k . v) :in *al-1* :with m = (from-alist i *al-1*) :do
    (is (eq v (lookup i m k))))
  (is (equal-alist *alist-10-latin*
                   (alist-from i (from-alist i *alist-10-latin*))))
  (is (equal-alist *alist-10-latin*
                   (alist-from i (from-alist i *alist-10-latin*))))
  (is (equal-alist *alist-100-decimal*
                       (alist-from i (from-alist i *al-1*))))
  (is (equal-alist *al-5*
                   (alist-from
                    i (check-invariant
                       i (join i (from-alist i *al-2*)
                               (from-alist i *al-3*))))))

  ;; insert
  (is (equal '((0)) (alist-from i (insert i (empty i) 0 nil))))
  (is (equal-alist
       '((1 . "1") (2 . "2") (3 . "3"))
       (alist-from i (insert i (from-alist i '((1 . "1") (3 . "3"))) 2 "2"))))
  ;; insert and join
  (is (equal-alist
       '((0 . "0") (1 . "1") (2 . "2"))
       (alist-from i (insert i (join i (from-alist i '((1 . "1")))
                                     (from-alist i'((2 . "2")))) 0 "0"))))
  ;; insert and size
  (is (= 101 (size i (insert i (from-alist i *al-1*) 101 "101"))))

  ;; drop
  (is (equal '(nil nil nil)
             (multiple-value-list (drop i (empty i) 0))))
  (multiple-value-bind (r d b)
      (drop i (from-alist i '((1 . "1") (2 . "2"))) 1)
    (is (equal '(((2 . "2")) "1" t)
               (list (alist-from i r) d b))))
  (multiple-value-bind (r d b)
      (drop i (from-alist i *al-1*) 42)
    (is (equal d "42")
    (is (equal b t)))
    (is (= (size i r) 99)))
  ;; drop and size
  (multiple-value-bind (r d b)
      (drop i (from-alist i *alist-100-decimal*) 57)
    (is (= (size i r) 99))
    (is (equal d "57"))
    (is (eql b t)))

  ;; first-key-value
  (is (equal '(nil nil nil)
             (multiple-value-list (first-key-value i (empty i)))))
  (multiple-value-bind (k v b)
      (first-key-value i (from-alist i *al-2*))
    (multiple-value-bind (vv bb) (lookup <alist> *al-2* k)
      (is (equal b t))
      (is (equal bb t))
      (is (equal v vv))))
  (multiple-value-bind (k v b)
      (first-key-value i (from-alist i *alist-100-latin*))
    (multiple-value-bind (vv bb) (lookup <alist> *alist-100-latin* k)
      (is (equal b t))
      (is (equal bb t))
      (is (equal v vv))))

  ;; decons
  (is (equal '(() () () ()) (multiple-value-list (decons i (empty i)))))
  (multiple-value-bind (m k v b) (decons i (from-alist i *alist-10-latin*))
    (is (eq b t))
    (is (equal (list v t)
               (multiple-value-list (lookup <alist> *alist-10-latin* k))))
    (is (equal (list nil nil)
               (multiple-value-list (lookup i m k))))
    (is (= (size i m) 9)))

  ;; fold-left
  (is (eql nil (fold-left i (empty i) (constantly t) nil)))
  (is (eql t (fold-left i (empty i) (constantly t) t)))
  (is (equal-alist
       '((2 . "2") (1 . "1") (20 . "20") (30 . "30"))
      (alist-from i
                  (fold-left
                   i (from-alist i (make-alist 2))
                   #'(lambda (m k v) (insert i m k v))
                   (from-alist i '((20 . "20") (30 . "30")))))))
  ;; fold-left and size
  (is (= 100
         (size i
               (fold-left i (from-alist i *alist-100-decimal*)
                          #'(lambda (m k v) (insert i m k v))
                          (from-alist i *alist-100-latin*)))))

  ;; fold-right
  (is (eql nil (fold-right i (empty i) (constantly t) nil)))
  (is (eql t (fold-right i (empty i) (constantly t) t)))
  (is (equal-alist
       '((1 . "1") (2 . "2") (20 . "20") (30 . "30"))
       (alist-from i
                   (fold-right
                    i (from-alist i (make-alist 2))
                    #'(lambda (k v m) (insert i m k v))
                    (from-alist i '((20 . "20") (30 . "30")))))))

  ;; for-each
  (is (eql nil (while-collecting (c)
                 (for-each i (empty i) #'(lambda (k v) (c (cons k v)))))))
  (is (equal-alist
       *alist-10-latin*
       (while-collecting (c)
         (with-output-to-string (o)
           (for-each i (from-alist i *alist-10-latin*)
                     #'(lambda (k v) (c (cons k v))))))))
  (is (= 1129 (length (with-output-to-string (o)
                        (for-each i (from-alist i *alist-100-english*)
                                  #'(lambda (x y)
                                    (format o "~A~A" x y)))))))

  ;; join
  (is (equal '() (join i (empty i) (empty i))))
  (is (equal-alist '((1 . "1") (2 . "2") (5 . "5") (6 . "6"))
                   (alist-from
                    i
                    (join i
                          (from-alist i '((1 . "1") (2 . "2")))
                          (from-alist i '((5 . "5") (6 . "6")))))))
  ;; join and size
  (is (= 100 (size i
                   (join i
                         (from-alist i *alist-10-latin*)
                         (from-alist i *alist-100-latin*)))))

  ;; divide and join
  (is (equal '(nil nil) (multiple-value-list (divide i (empty i)))))
  (multiple-value-bind (x y)
      (divide i (from-alist i *alist-10-latin*))
    (is (equal-alist *alist-10-latin*
                     (append (alist-from i x) (alist-from i y)))))
  ;; divide and size
  (multiple-value-bind (x y)
      (divide i (from-alist i '()))
    (is (empty-p i x))
    (is (empty-p i y)))
  (multiple-value-bind (x y)
      (divide i (from-alist i '((1 . "1"))))
    (is (empty-p i y))
    (is (= 1 (size i x))))
  (multiple-value-bind (x y)
      (divide i (from-alist i *alist-100-latin*))
    (let ((sx (size i x)) (sy (size i y)))
      (is (plusp sx))
      (is (plusp sy))
      (is (= 100 (+ sx sy)))))

  ;; size
  (is (= 0 (size i (empty i))))
  (is (= 100 (size i (from-alist i *alist-100-decimal*))))
  (is (= 99 (size i (decons i (from-alist i *alist-100-decimal*)))))

  ;; join/list
  ;; TODO: add tests


  ;; divide/list
  ;; TODO: add more tests
  (is (null (divide/list i (empty i))))

  ;; update-key
  ;; TODO: add more tests
  (is (null (update-key i (empty i) 0 (constantly nil))))

  ;; map/2
  ;; TODO: add more tests
  (is (null (map/2 i (constantly t) (empty i) (empty i))))

  ;; convert
  (is (null (convert <alist> i (empty i))))
  (is (equal-alist *alist-10-latin*
                   (convert <alist> i (convert i <alist> *alist-10-latin*))))
  t)

(defmethod interface-test :after ((i <number-map>))
  (let* ((a1 (make-alist 1000 "~@R"))
         (a2 (shuffle-list a1))
         (m1 (convert i <alist> a1))
         (m2 (convert i <alist> a2)))
    (check-invariant i m1)
    (check-invariant i m2)
    (is (= 10 (pure::node-height m1)))
    (is (<= 10 (pure::node-height m2) 15))
    (is (= 1000 (size i m1)))
    (is (= 1000 (size i m2)))))

(defparameter <denm> (<encoded-key-map>
                      :base-interface <number-map>
                      :key-encoder #'(lambda (dk) (* dk 2))
                      :key-decoder #'(lambda (ek) (/ ek 2))))

(deftest test-pure-map-interfaces ()
  (dolist (i (list <alist> <number-map> <hash-table> <fmim> <denm>))
    (interface-test i)))
