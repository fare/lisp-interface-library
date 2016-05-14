;;;;; From Interfaces to Classes: "Classy" examples of stateful classes.

(uiop:define-package :lil/transform/classy
  (:nicknames :classy)
  (:use :closer-common-lisp
        :lil/transform/classify :lil/interface/box)
  (:mix :fare-utils :uiop :alexandria)
  (:import-from :lil/stateful/tree)
  (:import-from :lil/stateful/map)
  (:import-from :lil/stateful/queue)
  (:export
   #:object-box
   #:>map< #:>number-map<
   #:>queue< #:>fifo-queue<
   #:empty-fifo-queue #:create-fifo-queue
   #:empty-number-map #:create-number-map
   #:check-invariant #:convert #:copy
   #:empty #:empty-p #:empty! #:make #:create #:contents #:size #:size<=n-p
   #:get-entry #:has-key-p #:first-entry #:entry-values #:encode-key #:decode-key
   #:singleton-p #:singleton #:singleton*
   ;; map functions
   #:lookup #:insert #:drop #:first-key-value #:decons
   #:fold-left #:fold-right #:fold-left* #:fold-right* #:for-each #:for-each*
   #:join #:divide #:join/list #:divide/list #:map/2 #:update-key
   ;;; Queue functions
   #:enqueue #:dequeue #:enqueue-first #:enqueue-last #:enqueue-many #:dequeue-all)
  (:documentation "traditional classy APIs for LIL data structure"))
(in-package :lil/transform/classy)

(in-package :classy)

(defmethod empty-p ((table hash-table))
  (zerop (hash-table-count table)))

(defclass object-box (box!)
  ((interface :reader class-interface)))

(define-classified-interface-class
  >map< (object-box) lil/stateful/map:<map>
  ((interface :initarg :interface))
  (:interface-argument (lil/stateful/map:<map> lil/stateful/map:<map>)))

(define-classified-interface-class
  >number-map< (>map<) lil/stateful/tree:<number-map>
  ((interface :initform lil/stateful/tree:<number-map> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -number-map))


(define-classified-interface-class
  >queue< (object-box) lil/stateful/queue:<queue>
  ((interface :initarg :interface))
  (:interface-argument (lil/stateful/queue:<queue> lil/stateful/queue:<queue>)))

(define-classified-interface-class
  >fifo-queue< (>queue<) lil/stateful/queue:<fifo-queue>
  ((interface :initform lil/stateful/queue:<fifo-queue> :allocation :class))
  (:interface-keyword nil) (:constructor-suffix -fifo-queue))

(defmethod print-object ((q >queue<) stream)
  (print-unreadable-object (q stream :type t :identity t)
    (write (contents q) :stream stream)))
