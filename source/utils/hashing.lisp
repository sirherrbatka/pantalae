(cl:in-package #:pantalea.utils.hashing)


(declaim (inline hash-integer))
(-> hash-integer (integer &optional (unsigned-byte 64)) (values (unsigned-byte 64)
                                                                (unsigned-byte 64)))
(defun hash-integer (n &optional (multi #x2545F4914F6CDD1D))
  "Attempts to randomize bits. Uses xorshift* algorithm."
  (let* ((new-state (~> (xorshift n 12) (xorshift -25) (ldb (byte 64 0) _)
                        (xorshift 27) (ldb (byte 64 0) _))))
    (values (ldb (byte 64 0) (* new-state multi))
            new-state)))

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
