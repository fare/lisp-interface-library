
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

(asdf:defsystem :interface/monad/identity
  :depends-on
  (:interface/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components ((:file "identity")))))))

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

(asdf:defsystem :interface/zero-plus
  :depends-on
  (:interface)
  :components
  ((:module "interface" :components ((:file "zero-plus")))))

(asdf:defsystem :interface/monad/maybe
  :depends-on
  (:interface/zero-plus :interface/monad)
  :components
  ((:module "interface" :components
    ((:module "monad" :components ((:file "maybe")))))))

(asdf:defsystem :interface/monad/test/monad/maybe
   :depends-on
   (:interface/monad/test/monad :interface/monad :interface/monad/maybe)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "maybe")))))))))))

 (asdf:defsystem :interface/monad/test/monad/monads
   :depends-on
   (:interface/monad/test/monad/maybe :interface/monad/test/monad/identity
    :interface/monad/test/monad)
   :components
   ((:module "interface" :components
     ((:module "monad" :components
       ((:module "test" :components
         ((:module "monad" :components ((:file "monads")))))))))))

