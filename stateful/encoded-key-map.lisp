;;;;; Stateful (mutable) mapping where key is encoded.
(uiop:define-package :lil/stateful/encoded-key-map
  (:use :closer-common-lisp
        :lil/interface/definition
        :lil/interface/base)
  (:use-reexport
   :lil/stateful/map)
  (:export
   #:<encoded-key-map> #:<parametric-encoded-key-map>))
(in-package :lil/stateful/encoded-key-map)

(define-interface <encoded-key-map>
    (<encoded-key-collection> <map-foldable-from-*> <map>) ()
  (:abstract))

(define-interface <parametric-encoded-key-map> (<parametric-encoded-key-collection> <encoded-key-map>) ()
  (:parametric (&key base-interface key-encoder key-decoder
		     key-interface value-interface)
               (make-interface
                :base-interface base-interface
		:key-interface key-interface
		:value-interface value-interface
                :key-encoder key-encoder
                :key-decoder key-decoder)))
