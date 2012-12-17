
(asdf:defsystem :interface-monad
  :description
  "The <monad> done interface passing style"
  :depends-on
  (:interface/monad))

(asdf:defsystem :interface/monad
  :depends-on
  (:interface)
  :components
  ((:module "interface" :components ((:file "monad")))))

(asdf:defsystem :interface/zero-plus
  :depends-on
  (:interface)
  :components
  ((:module "interface" :components ((:file "zero-plus")))))

(asdf:defsystem :interface/run
   :depends-on
   (:interface)
   :components
   ((:module "interface" :components ((:file "run")))))

;; The Standard Monads

(asdf:defsystem :interface/monad/identity
  :depends-on
  (:interface/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components ((:file "identity")))))))

(asdf:defsystem :interface/monad/maybe
  :depends-on
  (:interface/zero-plus :interface/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components ((:file "maybe")))))))

(asdf:defsystem :interface/monad/list
  :depends-on
  (:interface/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components ((:file "list")))))))

(asdf:defsystem :interface/monad/state
  :depends-on
  (:interface/run :interface/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components ((:file "state")))))))

(asdf:defsystem :interface/monad/continuation
   :depends-on
   (:interface/run :interface/monad)
   :components
   ((:module "interface" :components
     ((:module "monad" :components ((:file "continuation")))))))

;; The CHECK-INVARIANT test suite

(asdf:defsystem :interface/monad/test/monad
   :depends-on
   (:interface/monad)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components ((:file "monad")))))))))

(asdf:defsystem :interface/monad/test/monad/identity
  :depends-on
  (:interface/monad :interface/monad/identity
   :interface/monad/test/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components
      ((:module "test" :components
        ((:module "monad" :components ((:file "identity")))))))))))

(asdf:defsystem :interface/monad/test/monad/maybe
   :depends-on
   (:interface/monad/test/monad :interface/monad :interface/monad/maybe)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "maybe")))))))))))

(asdf:defsystem :interface/monad/test/monad/list
   :depends-on
   (:interface/monad/test/monad :interface/monad :interface/monad/list)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "list")))))))))))

(asdf:defsystem :interface/monad/test/monad/state
   :depends-on
   (:interface/monad/test/monad :interface/monad :interface/monad/state
    :interface/run)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "state")))))))))))

(asdf:defsystem :interface/monad/test/monad/continuation
   :depends-on
   (:interface/monad/test/monad :interface/monad :interface/monad/continuation
    :interface/run)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "continuation")))))))))))

(asdf:defsystem :interface/monad/test/monad/monads
   :depends-on
   (:interface/monad/test/monad/continuation :interface/monad/test/monad/state
    :interface/monad/test/monad/list :interface/monad/test/monad/maybe
    :interface/monad/test/monad/identity :interface/monad/test/monad)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "monads")))))))))))

