#-asdf3.1 (error "LIL requires ASDF 3.1 or later. Please upgrade your ASDF.")

(defsystem "lil"
  :description "LIL: Lisp Interface Library"
  :long-description
  "LIL is a collection of pure and stateful data structures in Interface Passing Style.
See http://fare.livejournal.com/155094.html regarding Interface Passing Style,
or our ILC'2012 article http://github.com/fare/lil-ilc2012/
"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :version (:read-file-form "version.text")
  :class :package-inferred-system
  :depends-on ("lil/core/all"
               "lil/interface/all"
               "lil/pure/all"
               "lil/stateful/all"
               "lil/transform/all"
               (:version "fare-memoization" "1.2.0"))
  :in-order-to ((test-op (test-op "lil/test"))))

(defsystem "lil/test" :depends-on ("lil/test/all")
  :perform (test-op (o c) (symbol-call :lil/test/all :test-suite)))


(register-system-packages "lil/core/all" '(:lil/core))
(register-system-packages "lil/interface/all" '(:interface))
(register-system-packages "lil/pure/all" '(:pure))
(register-system-packages "lil/stateful/all" '(:stateful))
(register-system-packages "lil/transform/classy" '(:classy))
(register-system-packages "lil/transform/posh" '(:posh))
(register-system-packages "lil/test/all" '(:lil/test))

(register-system-packages
 "closer-mop"
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))
