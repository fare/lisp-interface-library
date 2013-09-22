;;; Interface Passing Style : Run
(defpackage :interface/run
  (:nicknames :drewc.org/ips/run)
  (:use :cl)
  (:import-from :interface 
		#:define-interface
		#:<type>)
  (:export #:<run>
	   #:run))

(in-package :drewc.org/ips/run)

(interface:define-interface <run> (<type>)
  ()
  (:abstract)
  (:generic run (<run> runable &rest args)))




