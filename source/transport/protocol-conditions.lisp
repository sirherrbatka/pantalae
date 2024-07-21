(cl:in-package #:pantalea.transport.protocol)


(define-condition networking-already-present (error)
  ())

(define-condition cryptographic-handshake-failed (error)
  ())
