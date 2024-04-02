(cl:in-package #:pantalea.utils.hashing)


(declaim (inline hash-vector))
(defun hash-vector (input)
  (declare (type vector input))
  (iterate
    (declare (type (unsigned-byte 64) elt seed))
    (with seed = #x5555555555555555)
    (for elt in-vector input)
    (setf seed (~> (logxor seed elt) hash-integer))
    (finally (return seed))))

(declaim (inline hash-key))
(defun hash-key (public-key)
  (~> public-key ironclad:curve25519-key-y hash-vector))
