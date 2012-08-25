#+xcvb (module (:depends-on ("linearize/linearize" "pure/map-interface" "stateful/map-interface")))

(in-package :pure)

(interface::define-linearized-interface <linearized-map> (<map>) (stateful:<map>)
  ()
  (:parametric (interface #|&key unsafe|#)
    (make-interface :stateful-interface interface
                    #|:box-interface (if unsafe <value-box> <one-use-value-box>)|#)))
