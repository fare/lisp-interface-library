(in-package :pure)

(define-interface <linearized-map> (<map>)
  ((linearized-stateful-interface :reader linearized-stateful-interface)))
