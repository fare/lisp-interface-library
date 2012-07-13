;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; Functional mapping where key is encoded.
#+xcvb (module (:depends-on ("pure/map")))

(in-package :pure)

;;; This ought to have been possible with some type-directed metaprogramming...

(macrolet
    ;; non-hygienic: i from context.
    ((kv (form)
       (with-gensyms (k v)
         `(multiple-value-bind (,k ,v) ,form
            (values (decode-key i ,k) ,v))))
     (kvf (form)
       (with-gensyms (k v f)
         `(multiple-value-bind (,k ,v ,f) ,form
            (if ,f (values (decode-key i ,k) ,v t) (values nil nil nil)))))
     (mkvf (form)
       (with-gensyms (m k v f)
         `(multiple-value-bind (,m ,k ,v ,f) ,form
            (if ,f (values ,m (decode-key i ,k) ,v t) (values ,m nil nil nil)))))
     (ki ()
       '(key-interface i))
     ;; (mki ()
     ;;  '(mapped-key-interface i))
     ;; (vi ()
     ;;  '(value-interface i)) |#
     (bi ()
       '(base-interface i)))

  (defmethod check-invariant ((i <encoded-key-map>) m &key)
    (check-invariant (bi) m))
  (defmethod empty ((i <encoded-key-map>))
    (empty (bi)))
  (defmethod empty-p ((i <encoded-key-map>) map)
    (empty-p (bi) map))
  (defmethod lookup ((i <encoded-key-map>) map key)
    (lookup (bi) map (encode-key i key)))
  (defmethod insert ((i <encoded-key-map>) map key value)
    (insert (bi) map (encode-key i key) value))
  (defmethod drop ((i <encoded-key-map>) map key)
    (drop (bi) map (encode-key i key)))
  (defmethod first-key-value ((i <encoded-key-map>) map)
    (kvf (first-key-value (bi) map)))
  (defmethod decons ((i <encoded-key-map>) map)
    (mkvf (decons (bi) map)))
  (defmethod fold-left ((i <encoded-key-map>) map f seed)
    (fold-left (bi) map #'(lambda (acc k v) (funcall f acc (decode-key i k) v)) seed))
  (defmethod fold-right ((i <encoded-key-map>) map f seed)
    (fold-right (bi) map #'(lambda (k v acc) (funcall f (decode-key i k) v acc)) seed))
  (defmethod for-each ((i <encoded-key-map>) map f)
    (for-each (bi) map #'(lambda (k v) (funcall f (decode-key i k) v))))
  (defmethod join ((i <encoded-key-map>) map1 map2)
    (join (bi) map1 map2))
  (defmethod divide ((i <encoded-key-map>) map)
    (divide (bi) map))
  (defmethod size ((i <encoded-key-map>) map)
    (size (bi) map))
  (defmethod join/list ((i <encoded-key-map>) maplist)
    (join/list (bi) maplist))
  (defmethod divide/list ((i <encoded-key-map>) map)
    (divide/list (bi) map))
  (defmethod update-key ((i <encoded-key-map>) map key fun)
    (update-key (bi) map (encode-key i key) fun))
  (defmethod map/2 ((i <encoded-key-map>) fun map1 map2)
    (map/2 (bi) #'(lambda (k v1 f1 v2 f2)
                    (funcall fun (decode-key i k) v1 f1 v2 f2))
           map1 map2)))

(defun <encoded-key-map> (&key base-interface key-encoder key-decoder)
  (<parametric-encoded-key-map>
   :base-interface base-interface :key-encoder key-encoder :key-decoder key-decoder))

(defmethod encode-key ((i <parametric-encoded-key-map>) k)
  (funcall (key-encoder i) k))

(defmethod decode-key ((i <parametric-encoded-key-map>) k)
  (funcall (key-decoder i) k))
