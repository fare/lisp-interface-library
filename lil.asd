;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(in-package :asdf)

(defsystem :lil
  :description "LIL: Lisp Interface Library"
  :long-description
  "LIL is a collection of pure and stateful data structures in Interface Passing Style.
See http://fare.livejournal.com/155094.html regarding Interface Passing Style,
or our ILC'2012 article http://github.com/fare/lil-ilc2012/
"
  :class :package-system
  :defsystem-depends-on
  #.(unless (find-class :package-system nil) '(:asdf-package-system))
  :depends-on (:lil/interface/all
               :lil/pure/all
               :lil/stateful/all
               :lil/transform/all)
  :perform (test-op :after (o c)
             (load-system :lisp-interface-library-test)
             (symbol-call :lisp-interface-library-test :test-suite)))

(register-system-packages :lil/pure/all '(:pure))
(register-system-packages :lil/stateful/all '(:stateful))
(register-system-packages :lil/transform/classy '(:classy))
(register-system-packages :lil/transform/posh '(:posh))

(register-system-packages
 :closer-mop
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))
