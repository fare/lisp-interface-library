
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

