#+xcvb (module (:depends-on ("linearize/linearize" "pure/map-interface" "stateful/map-interface")))

(in-package :pure)

(define-interface <linearized-map> (<linearized> <map>)
  ()
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))
