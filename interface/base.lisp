;;;;; Basic Interfaces

(uiop:define-package :lil/interface/base
  (:use :closer-common-lisp :lil/core)
  (:export
   ;; Base
   #:<type> #:check-invariant #:convert
   #:<any>
   #:<classy>

   ;; Base interfaces
   #:<copyable> #:copy #:<copy-is-identity>
   ;; #:update ;; TODO: move to pure?
   #:<has-base-interface> #:base-interface

   #:<makeable> #:make #:create #:contents))
(in-package :lil/interface/base)

(define-interface <type> (<interface>) ()
  (:documentation "An interface encapsulating a particular type of objects")
  (:abstract)
  (:generic> check-invariant (object &key #+sbcl &allow-other-keys)
   (:in 1) (:values object) (:out nil) ;; :out nil because normal methods don't return anything!
   (:documentation "Check whether an OBJECT fulfills the invariant(s) required
to be an object of the type represented by this interface.
On success the OBJECT itself is returned. On failure an error is signalled.")
   (:method> :around (object &key #+sbcl &allow-other-keys)
      (call-next-method)
      object))
  (:generic convert (<destination> <origin> object)
   (:values object) (:out 0)
   (:documentation "Convert an OBJECT following interface <ORIGIN>
    into a new object following interface <DESTINATION>."))
  (:generic> create (contents &key #+sbcl &allow-other-keys)
   (:values object) (:out 0)
   (:documentation "create an object conforming to the interface
based on CONTENTS and provided keyword options, returning the object."))
  (:generic> contents (object &key #+sbcl &allow-other-keys)
   (:values contents) (:in 1)
   (:documentation "Given an object, return contents sufficient to re-CREATE a similar object.")))


(define-interface <any> (<type>) ()
  ;; sometimes useful to pass as parameter to a parametrically-polymorphic interface
  (:singleton)
  (:documentation "Concrete interface for any object")
  (:method> check-invariant (object &key) (declare (ignore object)) (values))
  (:method> convert (<origin> object) (declare (ignore <origin>)) object))


(define-interface <copyable> (<type>) ()
  (:documentation "A type of objects that can be copied")
  (:abstract)
  (:generic> copy (object)
   (:in 1) (:values object) (:out 0)
   (:documentation "Copy an OBJECT, returning a fresh object with
equivalent contains.
Beware: how deep the copy goes depends on the interface;
copying a data structure may lead to identical objects rather than copies
being left at leaves; or it may not.
If you work in a stateful style, then this matters a lot
because your side-effects may or may not be seen by more or fewer copies
than you think. Please consult the documentation of appropriate methods.")))

(define-interface <copy-is-identity> (<copyable>) ()
  (:abstract)
  (:method> copy (x)
    x)
  (:documentation "Pure Persistent Data Structures have trivial copying"))

;;; This one is only colloquial for use in pure datastructure. TODO: Move it to pure-?
;;(defgeneric update (<type> object &key)
;;  (:documentation "Update OBJECT by overriding some of its slots
;;  with those specified as initarg keywords, returning a new object."))

(define-interface <has-base-interface> (<interface>) ()
  (:generic> base-interface () (:values base-interface)
    (:documentation "from a functor, extract the base interface parameter")))


;;; Makeable
(define-interface <makeable> (<type>) ()
  (:generic> make (&key #+sbcl &allow-other-keys)
   (:values object) (:out 0)
   ;; the #+sbcl works around SBCL bug https://bugs.launchpad.net/sbcl/+bug/537711
   (:documentation "create an object conforming to the interface
based on provided initarg keywords, returning the object.")))


;;; Classy Interface (i.e. has some associated class)

(define-interface <classy> (<makeable>)
  ((class :reader interface-class :allocation :class))
  (:method> make (&rest keys &key #+sbcl &allow-other-keys)
     (apply 'make-instance (interface-class <classy>) keys)))
