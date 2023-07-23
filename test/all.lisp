;;;;; All tests
(defpackage :lil/test/all
  (:nicknames :lil/test)
  (:use :lil/test/base)
  (:import-from :lil/test/pure-map)
  (:import-from :lil/test/pure-lexicographic)
  (:import-from :lil/test/stateful-map)
  (:export #:test-suite))
