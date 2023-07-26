;;; "Fast Mergable Integer Maps"
;;; See article of same name by Chris Okasaki & Andrew Gill, 1998
;;; http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
;;; Under the hood: Big Endian Patricia Trees (Tries).
;;; Note however that in our API, what they call "merge" is called "join".

(uiop:define-package :lil/pure/fmim-implementation
  (:use :closer-common-lisp
        :lil/core
        :lil/interface/base
        :lil/interface/box
        :lil/pure/tree)
  (:use-reexport
   :lil/pure/fmim))
(in-package :lil/pure/fmim-implementation)

(defmethod check-invariant ((i <fmim>) (map trie-head) &key)
  (trie-check-invariant (box-ref map) (node-height map) 0))

(defun trie-check-invariant (trie position key)
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (check-type position (unsigned-byte))
  (check-type key (unsigned-byte))
  (assert (zerop (ldb (byte position 0) key)))
  (unless (zerop position)
    (etypecase trie
      (trie-skip
       (let ((pbits (node-prefix-bits trie))
             (plen (node-prefix-length trie)))
         (check-type pbits (unsigned-byte))
         (check-type plen (integer 1 *))
         (assert (<= (integer-length pbits) plen))
         (assert (<= plen position))
         (let ((pos (- position plen)))
           (trie-check-invariant (box-ref trie) pos (dpb pbits (byte plen pos) key)))))
       (trie-branch
         (let ((pos (1- position)))
           (trie-check-invariant (left trie) pos key)
           (trie-check-invariant (right trie) pos (dpb 1 (byte 1 pos) key))))))
  (values))

(defmethod lookup ((i <fmim>) map key)
  (check-type map (or null trie-head))
  (check-type key (integer 0 *))
  (if map
      (let ((len (integer-length key))
            (height (node-height map)))
        (if (< height len)
            (values nil nil)
            (trie-lookup (box-ref map) height key)))
      (values nil nil)))

(defun trie-lookup (trie position key)
  (cond
    ((zerop position) (values trie t))
    ((null trie) (values nil nil))
    (t
     (check-type trie trie-node)
     (assert (plusp position))
     (etypecase trie
       (trie-skip
        (let* ((pbits (node-prefix-bits trie))
               (plen (node-prefix-length trie))
               (pos (- position plen)))
          (if (= pbits (ldb (byte plen pos) key))
              (trie-lookup (box-ref trie) pos key)
              (values nil nil))))
       (trie-branch
        (let ((pos (1- position)))
          (trie-lookup
           (if (zerop (ldb (byte 1 pos) key))
               (left trie)
               (right trie))
           pos key)))))))

(defun make-trie-leaf (position key value)
  (if (zerop position)
      value
      (make-trie-skip position position (ldb (byte position 0) key) value)))

(defun make-trie-skip (position length bits datum)
  (cond
    ((zerop length)
     datum)
    ((and (plusp position) (null datum))
     nil)
    ((and (> position length) (typep datum 'trie-skip))
     (make-instance
      'trie-skip
      :prefix-length (+ length (node-prefix-length datum))
      :prefix-bits (dpb bits (byte length (node-prefix-length datum))
                        (node-prefix-bits datum))
      :value (box-ref datum)))
    (t
     (make-instance
      'trie-skip
      :prefix-length length
      :prefix-bits bits
      :value datum))))

(defun make-trie-branch (pos left right)
  (cond
    ((or (zerop pos)
         (and (typep left 'full-trie-branch)
              (typep right 'full-trie-branch)))
     (make-instance 'full-trie-branch :left left :right right))
    ((and left right)
     (make-instance 'trie-branch :left left :right right))
    (left
     (make-trie-skip pos 1 0 left))
    (right
     (make-trie-skip pos 1 1 right))
    (t
     nil)))

(defun make-trie-head (height trie)
  (cond
    ((and (plusp height) (null trie))
     nil)
    ((and (plusp height)
          (typep trie 'trie-skip)
          (zerop (ldb (byte 1 (1- (node-prefix-length trie))) (node-prefix-bits trie))))
     (let* ((plen (integer-length (node-prefix-bits trie)))
            (datum (box-ref trie))
            (height (- height (- (node-prefix-length trie) plen)))
            (trie (make-trie-skip height plen (node-prefix-bits trie) datum)))
       (make-instance 'trie-head :height height :value trie)))
    (t
     (make-instance 'trie-head :height height :value trie))))

(defmethod insert ((i <fmim>) map key value)
  (check-type map (or null trie-head))
  (check-type key (integer 0 *))
  (let ((len (integer-length key)))
    (multiple-value-bind (l d)
        (if (null map)
            (values len (make-trie-skip len len key value))
            (let ((height (node-height map))
                  (trie (box-ref map)))
              (if (< height len)
                  (values len
                          (make-trie-branch
                           len
                           (make-trie-skip len (- len height 1) 0 trie)
                           (make-trie-leaf (1- len) key value)))
                  (values height
                          (trie-insert trie height key value)))))
      (make-trie-head l d))))

(defun trie-insert (trie position key value)
  (if (zerop position) value
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (if (= pbits (ldb (byte plen pos) key))
               (make-trie-skip position plen pbits
                               (trie-insert (box-ref trie) pos key value))
               (let* ((datum (box-ref trie))
                      (len (1- plen))
                      (pos (1- position))
                      (trie1 (make-trie-skip
                              position len (ldb (byte len 0) pbits) datum))
                      (hb (ldb (byte 1 len) pbits))
                      (new-hb (ldb (byte 1 pos) key)))
                 (if (= hb new-hb)
                     (make-trie-skip
                      position 1 hb
                      (trie-insert trie1 pos key value))
                     (let ((leaf (make-trie-leaf pos key value)))
                       (if (zerop new-hb)
                           (make-trie-branch pos leaf trie1)
                           (make-trie-branch pos trie1 leaf))))))))
        (trie-branch
         (let ((pos (1- position)))
           (if (zerop (ldb (byte 1 pos) key))
               (make-trie-branch
                pos
                (trie-insert (left trie) pos key value)
                (right trie))
               (make-trie-branch
                pos
                (left trie)
                (trie-insert (right trie) pos key value))))))))

(defmethod drop ((i <fmim>) map key)
  (check-type map (or null trie-head))
  (multiple-value-bind (v f)
      (lookup i map key)
    (if f
        (values
         (multiple-value-bind (datum non-empty-p)
             (trie-drop (box-ref map) (node-height map) key)
           (when non-empty-p
             (make-trie-head (node-height map) datum)))
         v f)
        (values map nil nil))))

(defun trie-drop (trie position key)
  ;; from our contract with drop,
  ;; we do assume the key IS in fact in the trie.
  (if (zerop position)
      (values nil nil)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (assert (= pbits (ldb (byte plen pos) key)))
           (multiple-value-bind (datum non-empty-p)
               (trie-drop (box-ref trie) pos key)
             (if non-empty-p
                 (values (make-trie-skip position plen pbits datum) t)
                 (values nil nil)))))
        (trie-branch
         (let* ((pos (1- position))
                (bit (ldb (byte 1 pos) key)))
           (values
            (cond
              ((zerop pos)
               (make-trie-skip 1 1 (- 1 bit)
                               (if (zerop bit) (right trie) (left trie))))
              ((zerop bit)
               (make-trie-branch
                position
                (trie-drop (left trie) pos key)
                (right trie)))
              (t
               (make-trie-branch
                position
                (left trie)
                (trie-drop (right trie) pos key))))
            t))))))

(defmethod first-key-value ((i <fmim>) map)
  (leftmost i map))

(defmethod fold-left* ((i <fmim>) map f seed)
  (if (null map)
      seed
      (trie-fold-left (box-ref map) (node-height map) 0 f seed)))

(defun trie-fold-left (trie position key f seed)
  (if (zerop position)
      (funcall f seed key trie)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-fold-left
            (box-ref trie) pos (dpb pbits (byte plen pos) key) f seed)))
        (trie-branch
         (let ((pos (1- position)))
           (trie-fold-left
            (right trie) pos (dpb 1 (byte 1 pos) key) f
            (trie-fold-left
             (left trie) pos key f seed)))))))

(defmethod fold-right* ((i <fmim>) map f seed)
  (if (null map)
      seed
      (trie-fold-right (box-ref map) (node-height map) 0 f seed)))

(defun trie-fold-right (trie position key f seed)
  (if (zerop position)
      (funcall f key trie seed)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-fold-right
            (box-ref trie) pos (dpb pbits (byte plen pos) key) f seed)))
        (trie-branch
         (let ((pos (1- position)))
           (trie-fold-right
            (left trie) pos key f
            (trie-fold-right
             (right trie) pos (dpb 1 (byte 1 pos) key) f seed)))))))

(defmethod leftmost ((i <fmim>) map)
  (if (null map)
      (values nil nil nil)
      (trie-leftmost (box-ref map) (node-height map) 0)))

(defun trie-leftmost (trie position key)
  (if (zerop position)
      (values key trie t)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-leftmost
            (box-ref trie) pos (dpb pbits (byte plen pos) key))))
        (trie-branch
         (trie-leftmost (left trie) (1- position) key)))))

(defmethod rightmost ((i <fmim>) map)
  (if (null map)
      (values nil nil nil)
      (trie-rightmost (box-ref map) (node-height map) 0)))

(defun trie-rightmost (trie position key)
  (if (zerop position)
      (values key trie t)
      (etypecase trie
        (trie-skip
         (let* ((pbits (node-prefix-bits trie))
                (plen (node-prefix-length trie))
                (pos (- position plen)))
           (trie-rightmost
            (box-ref trie) pos (dpb pbits (byte plen pos) key))))
        (trie-branch
         (let ((pos (1- position)))
           (trie-rightmost (right trie) pos (dpb 1 (byte 1 pos) key)))))))

(defmethod divide ((i <fmim>) map)
  (etypecase map
    (null
     (values nil nil))
    (trie-head
     (let ((height (node-height map))
           (datum (box-ref map)))
       (if (zerop height)
           (values nil map)
           (etypecase datum
             (trie-branch
              (values
               (make-trie-head (1- height) (left datum))
               (make-trie-head height
                               (make-trie-skip height 1 1 (right datum)))))
             (trie-skip
              (let* ((pbits (node-prefix-bits datum))
                     (plen (node-prefix-length datum))
                     (position (- height plen)))
                (if (zerop position)
                    (values nil map)
                    (let ((child (box-ref datum)))
                      (etypecase child
                        (trie-branch
                         (flet ((f (bit datum)
                                  (make-trie-head height
                                                  (make-trie-skip
                                                   height
                                                   (1+ plen)
                                                   (dpb pbits (byte plen 1) bit)
                                                   datum))))
                           (values
                            (f 0 (left child))
                            (f 1 (right child))))))))))))))))

(defmethod has-key-p ((i <fmim>) map key) ;; TODO: make that more generic
  (nth-value 1 (lookup i map key)))
(defmethod singleton-p ((i <fmim>) map)
  (etypecase map
    (null nil)
    (trie-head (let ((height (node-height map)))
                 (or (zerop height)
                     (let ((trie (box-ref map)))
                       (etypecase trie
                         (trie-skip (= height (node-prefix-length trie)))
                         (trie-branch nil)
                         (full-trie-branch nil))))))))
;; TODO: implement the rest of the collection API

;;; The whole point of fmim is that we could do a fast "merge",
(defmethod join ((i <fmim>) a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t
     (check-type a trie-head)
     (check-type b trie-head)
     (let* ((ha (node-height a))
            (hb (node-height b))
            (h (max ha hb)))
       (make-trie-head
        h (trie-join (make-trie-skip h (- h ha) 0 (box-ref a))
                     (make-trie-skip h (- h hb) 0 (box-ref b))
                     h))))))

(defun trie-join (a b position)
  (if (zerop position) a
      (etypecase a
        (full-trie-branch a)
        (trie-branch
         (let ((pos (1- position)))
           (etypecase b
             (trie-branch
              (make-trie-branch
               position
               (trie-join (left a) (left b) pos)
               (trie-join (right a) (right b) pos)))
             (trie-skip
              (let* ((pbits (node-prefix-bits b))
                     (plen (node-prefix-length b))
                     (bh (ldb (byte 1 (1- plen)) pbits))
                     (b1
                      (make-trie-skip
                       pos (1- plen)
                       (ldb (byte (1- plen) 0) pbits) (box-ref b))))
                (if (zerop bh)
                    (make-trie-branch
                     position (trie-join (left a) b1 pos) (right a))
                    (make-trie-branch
                     position (left a) (trie-join (right a) b1 pos))))))))
        (trie-skip
         (let* ((pbits (node-prefix-bits a))
                (plen (node-prefix-length a))
                (pos (1- position))
                (ah (ldb (byte 1 (1- plen)) pbits))
                (a1
                 (make-trie-skip
                  pos (1- plen)
                  (ldb (byte (1- plen) 0) pbits) (box-ref a))))
           (etypecase b
             (trie-branch
                (if (zerop ah)
                    (make-trie-branch
                     position (trie-join a1 (left b) pos) (right b))
                    (make-trie-branch
                     position (left b) (trie-join a1 (right b) pos))))
             (trie-skip
              (let* ((pbitsb (node-prefix-bits b))
                     (plenb (node-prefix-length b))
                     (bh (ldb (byte 1 (1- plenb)) pbitsb))
                     (b1
                      (make-trie-skip
                       pos (1- plenb)
                       (ldb (byte (1- plenb) 0) pbitsb) (box-ref b))))
                (if (= ah bh)
                    (make-trie-skip position 1 0 (trie-join a1 b1 pos))
                    (if (zerop ah)
                        (make-trie-branch position a1 b1)
                        (make-trie-branch position b1 a1)))))))))))

(defmethod print-object ((x trie-head) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~D ~S" (node-height x) (box-ref x))))

(defmethod print-object ((x trie-skip) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~v,'0B ~S" (node-prefix-length x) (node-prefix-bits x) (box-ref x))))

(defmethod print-object ((x trie-branch) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~S ~S" (left x) (right x))))
