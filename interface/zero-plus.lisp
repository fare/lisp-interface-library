;;; Interface Passing Style : Zero/plus

(defpackage :interface/zero-plus
  (:nicknames :drewc.org/ips/zero-plus)
  (:use :cl)
  (:import-from :interface
		#:define-interface
		#:<type>)
  (:export #:<zero-plus>
	   #:zero
	   #:plus))
(in-package :drewc.org/ips/zero-plus)

(interface:define-interface <zero-plus> (<type>)
  ()
  (:abstract)
  (:generic zero (<zero-plus>))
  (:generic plus (<zero-plus> a b)))
