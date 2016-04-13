(uiop:define-package :lil/test/pure-map
  (:import-from :stateful)
  (:use :pure :lil/test/base :lil/transform/linearized-map :lil/interface/base
        :cl :uiop :fare-utils :hu.dwim.stefil)
  (:export :read-only-linear-map-test))

(in-package :lil/test/pure-map)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-pure-map
            :in test-suite
            :documentation "Testing pure functional maps"))

(defmethod interface-test ((i <map>))
  (read-only-linear-map-test i)
  (simple-linear-map-test i)
  (harder-linear-map-test i)
  (multilinear-map-test i))

(defmethod read-only-linear-map-test ((i lil/interface/map:<map>))
  (declare (optimize (speed 1) (debug 3) (space 3)))
  ;;; TODO: test each and every function in the API
  (X 'read-only-linear-map-test *package* i)
  (X 'empty)
  (is (null (map-alist* i (empty i))))
  (is (empty-p i (alist-map* i ())))

  (X 'lookup)
  (is (equal "12"
             (lookup
              i
              (alist-map*
               i '((57 . "57") (10 . "10") (12 . "12")))
              12)))
  (loop :for (k . v) :in *al-1* :with m = (alist-map* i *al-1*) :do
    (is (eq v (lookup i m k))))

  (X 'alist-map*-and-back)
  (is (equal-alist *alist-10-latin*
                   (map-alist* i (alist-map* i *alist-10-latin*))))
  (is (equal-alist *alist-10-latin*
                   (map-alist* i (alist-map* i *alist-10-latin*))))
  (is (equal-alist *alist-100-decimal*
                   (map-alist* i (alist-map* i *al-1*))))

  (X 'first-key-value)
  (is (equal '(nil nil nil)
             (multiple-value-list (first-key-value i (empty i)))))
  (multiple-value-bind (k v b)
      (first-key-value i (alist-map* i *al-2*))
    (multiple-value-bind (vv bb) (lookup <alist> *al-2* k)
      (is (equal b t))
      (is (equal bb t))
      (is (equal v vv))))
  (multiple-value-bind (k v b)
      (first-key-value i (alist-map* i *alist-100-latin*))
    (multiple-value-bind (vv bb) (lookup <alist> *alist-100-latin* k)
      (is (equal b t))
      (is (equal bb t))
      (is (equal v vv))))

  (X 'fold-left)
  (is (eql nil (fold-left i (empty i) (constantly t) nil)))
  (is (eql t (fold-left i (empty i) (constantly t) t)))

  (X 'fold-left*)
  (is (eql nil (fold-left* i (empty i) (constantly t) nil)))
  (is (eql t (fold-left* i (empty i) (constantly t) t)))

  (X 'fold-right)
  (is (eql nil (fold-right i (empty i) (constantly t) nil)))
  (is (eql t (fold-right i (empty i) (constantly t) t)))

  (X 'fold-right*)
  (is (eql nil (fold-right* i (empty i) (constantly t) nil)))
  (is (eql t (fold-right* i (empty i) (constantly t) t)))

  (X 'size)
  (is (= 0 (size i (empty i))))
  (is (= 100 (size i (alist-map* i *alist-100-decimal*))))

  (X 'for-each)
  (is (equal-alist
       *alist-10-latin*
       (while-collecting (c)
	 (for-each i (alist-map* i *alist-10-latin*) #'c))))

  (X 'for-each*)
  (is (eql nil (while-collecting (c)
                 (for-each* i (empty i) #'(lambda (k v) (c (cons k v)))))))
  (is (= 1129 (length (with-output-to-string (o)
                        (for-each* i (alist-map* i *alist-100-english*)
                                  #'(lambda (x y)
                                      (format o "~A~A" x y)))))))

  t)

(defmethod simple-linear-map-test ((i <map>))
  (declare (optimize (speed 1) (debug 3) (space 3)))
  ;;; TODO: test each and every function in the API
  (X 'simple-linear-map-test *package* i)

  (X 'insert)
  (is (equal '((0)) (map-alist* i (insert i (empty i) 0 nil))))
  (is (equal-alist
       '((1 . "1") (2 . "2") (3 . "3"))
       (map-alist* i (insert i (alist-map* i '((1 . "1") (3 . "3"))) 2 "2"))))

  (X 'insert-and-join)
  (is (equal-alist
       '((0 . "0") (1 . "1") (2 . "2"))
       (map-alist* i (insert i (join i (alist-map* i '((1 . "1")))
				     (alist-map* i'((2 . "2")))) 0 "0"))))

  (X 'insert-and-size)
  (is (= 101 (size i (insert i (alist-map* i *al-1*) 101 "101"))))

  (X 'drop)
  (multiple-value-bind (m k v) (drop i (empty i) 0)
    (is (empty-p i m))
    (is (null k))
    (is (null v)))
  (multiple-value-bind (r d b)
      (drop i (alist-map* i '((1 . "1") (2 . "2"))) 1)
    (is (equal '(((2 . "2")) "1" t)
               (list (map-alist* i r) d b))))
  (multiple-value-bind (r d b)
      (drop i (alist-map* i *al-1*) 42)
    (is (equal d "42")
        (is (equal b t)))
    (is (= (size i r) 99)))

  (X 'drop-and-size)
  (multiple-value-bind (r d b)
      (drop i (alist-map* i *alist-100-decimal*) 57)
    (is (= (size i r) 99))
    (is (equal d "57"))
    (is (eql b t)))

  (X 'decons)
  (multiple-value-bind (b m k v) (decons i (empty i))
    (is (empty-p i m))
    (is (equal '(nil nil nil) (list b k v))))
  (multiple-value-bind (b m k v) (decons i (alist-map* i *alist-10-latin*))
    (is (eq b t))
    (is (equal (list v t)
               (multiple-value-list (lookup <alist> *alist-10-latin* k))))
    (is (equal (list nil nil)
               (multiple-value-list (lookup i m k))))
    (is (= (size i m) 9)))

  (X 'fold-left*)
  (is (equal-alist
       '((2 . "2") (1 . "1") (20 . "20") (30 . "30"))
       (map-alist* i
		   (fold-left*
		    i (alist-map* i (make-alist 2))
		    #'(lambda (m k v) (insert i m k v))
		    (alist-map* i '((20 . "20") (30 . "30")))))))

  (X 'fold-left*-and-size)
  (is (= 100
         (size i
               (fold-left* i (alist-map* i *alist-100-decimal*)
			   #'(lambda (m k v) (insert i m k v))
			   (alist-map* i *alist-100-latin*)))))

  (X 'fold-right*)
  (is (equal-alist
       '((1 . "1") (2 . "2") (20 . "20") (30 . "30"))
       (map-alist* i
		   (fold-right*
		    i (alist-map* i (make-alist 2))
		    #'(lambda (k v m) (insert i m k v))
		    (alist-map* i '((20 . "20") (30 . "30")))))))

  (X 'join)
  (is (equal-alist *al-5*
                   (map-alist*
                    i (check-invariant
                       i (join i (alist-map* i *al-2*)
                               (alist-map* i *al-3*))))))
  (is (empty-p i (join i (empty i) (empty i))))
  (is (equal-alist '((1 . "1") (2 . "2") (5 . "5") (6 . "6"))
                   (map-alist*
                    i
                    (join i
                          (alist-map* i '((1 . "1") (2 . "2")))
                          (alist-map* i '((5 . "5") (6 . "6")))))))
  (X 'join-and-size)
  (is (= 100 (size i
                   (join i
                         (alist-map* i *alist-10-latin*)
                         (alist-map* i *alist-100-latin*)))))

  (X 'divide-and-join)
  (multiple-value-bind (m1 m2) (divide i (empty i))
    (is (empty-p i m1))
    (is (empty-p i m2)))
  (multiple-value-bind (x y)
      (divide i (alist-map* i *alist-10-latin*))
    (is (equal-alist *alist-10-latin*
                     (append (map-alist* i x) (map-alist* i y)))))

  (X 'divide-and-size)
  (multiple-value-bind (x y)
      (divide i (alist-map* i '()))
    (is (empty-p i x))
    (is (empty-p i y)))
  (multiple-value-bind (x y)
      (divide i (alist-map* i '((1 . "1"))))
    (is (empty-p i x))
    (is (= 1 (size i y))))
  ;; Repeatedly divide a map into pairs of smaller sub-maps.
  ;; TODO: implement and use a queue interface?
  (loop with q = (list (alist-map* i *alist-100-latin*))
        while q do
          (nest
           (let* ((m (pop q))
                  (s (size i m))))
           (multiple-value-bind (x y) (divide i m))
           (let ((sx (size i x))
                 (sy (size i y)))
             (is (= s (+ sx sy)))
             (is (if (zerop sx) (>= 1 sy) (plusp sy)))
             (unless (zerop sx) (push y q) (push x q)))))

  (X 'size-and-decons)
  (is (= 99 (size i (nth-value 1 (decons i (alist-map* i *alist-100-decimal*))))))

  (X 'update-key)
  ;; TODO: add more tests
  (is (empty-p i (update-key i (empty i) 0 (constantly nil))))

  (X 'map/2)
  ;; TODO: add more tests
  (is (empty-p i (map/2 i (constantly t) (empty i) (empty i))))

  (X 'convert)
  (is (null (convert <alist> i (empty i))))
  (is (equal-alist *alist-10-latin*
                   (convert <alist> i (convert i <alist> *alist-10-latin*))))

  (X 'iterator)
  (is (equal-alist *alist-10-latin*
                   (flow i <alist> (convert i <alist> *alist-10-latin*) nil)))

  t)

(defmethod harder-linear-map-test ((i <map>))
  ;; (X 'join/list)
  ;; TODO: add tests

  (X 'divide/list)
  (is (null (divide/list i (empty i))))
  ;; Repeatedly divide a map into pairs of smaller sub-maps.
  ;; TODO: implement and use a queue interface?
  (loop with q = (list (alist-map* i *alist-100-latin*))
        while q do
          (let* ((m (pop q))
                 (s (size i m))
                 (l (divide/list i m))
                 (sl (mapcar (lambda (x) (size i x)) l)))
            (is (= s (reduce #'+ sl :initial-value 0)))
            (every #'plusp sl)
            (when (cddr l) (setf q (append l q))))))


(defmethod multilinear-map-test ((i <map>))
  (let ((m (alist-map* i *alist-10-latin*)))
    (equal-alist (map-alist* i m) (map-alist* i (join i m m)))))

(defmethod simple-linear-map-test :after ((i <number-map>))
  (let* ((a1 (make-alist 200 "~@R"))
         (a2 (shuffle-list a1))
         (m1 (convert i <alist> a1))
         (m2 (convert i <alist> a2)))
    (check-invariant i m1)
    (check-invariant i m2)
    (is (= 8 (pure::node-height m1)))
    (is (<= 8 (pure::node-height m2) 15))
    (is (= 200 (size i m1)))
    (is (= 200 (size i m2)))))

(defparameter <denm> (<encoded-key-map>
                      :base-interface <number-map>
                      :key-encoder #'(lambda (dk) (* dk 2))
                      :key-decoder #'(lambda (ek) (/ ek 2))))

(deftest test-pure-map-interfaces ()
  (dolist (i (list <alist> <number-map> <hash-table> <fmim> <denm>))
    (interface-test i)))

(defparameter <lsnm> (<linearized-map> stateful:<number-map>))

(deftest test-linearized-map-interfaces ()
  (read-only-linear-map-test <lsnm>)
  (simple-linear-map-test <lsnm>)
  (harder-linear-map-test <lsnm>)
  (linear-string-map-test))

(deftest linear-string-map-test ()
  (let* ((a1 (loop :for i :from 1 :to 42 :collect (cons (format nil "~@R" i) i)))
         (a2 (shuffle-list a1))
         (m1 (convert <string-map> <alist> a1))
         (m2 (convert <string-map> <alist> a2))
         (b1 (convert <alist> <string-map> m1))
         (b2 (convert <alist> <string-map> m2)))
    (is (= 42
           (size <alist> a1)
           (size <alist> a2)
           (size <string-map> m1)
           (size <string-map> m2)
           (size <alist> b1)
           (size <alist> b2)))
    (is (equal b1 b2))))

(deftest regression-test-1--divide-number-map ()
  (multiple-value-bind (x y)
      (divide <number-map> (alist-map* <number-map> '((557088 . 7) (229378 . 79))))
    (is x)
    (is y)
    (destructuring-bind ((kx . vx)) (map-alist* <number-map> x)
      (destructuring-bind ((ky . vy)) (map-alist* <number-map> y)
	(when (>= kx ky) (rotatef kx ky) (rotatef vx vy))
	(is (equal (list kx vx ky vy) '(229378 79 557088 7)))))))
