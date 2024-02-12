#|
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2) Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#
(cl:in-package #:pantalea.cryptography)


(defun make-25519-public-key (private)
  (ic:make-public-key :curve25519 :y private))

(defun make-25519-private-key ()
  (ic:make-private-key :curve25519 :x (ic:random-data 32)))

(defun make-25519-keys ()
  (let* ((x (ic:random-data 32))
         (private (ic:make-private-key :curve25519 :x x))
         (y (ic:curve25519-key-y private))
         (public (ic:make-public-key :curve25519 :y y)))
    (make 'keys-pair :public public :private private)))

(defun chain-key-length (chain)
  (array-dimension (key chain) 0))

(defun exchange-25519-key (private-key public-key)
  (ic:diffie-hellman private-key public-key))

(defun padding-bytes-count (length)
  (- 16 (mod length 16)))

(defun make-padded-vector-for-length (length)
  (let ((padding-bytes-count (padding-bytes-count length)))
    (make-array (+ length padding-bytes-count)
                :element-type '(unsigned-byte 8)
                :initial-element padding-bytes-count)))

(defun make-padded-vector (vector)
  (make-padded-vector-for-length (length vector)))

(defun pkcs7-pad (vector)
  (declare (type (simple-array (unsigned-byte 8) (*)) vector))
  (iterate
    (with result = (make-padded-vector vector))
    (for i from 0 below (length vector))
    (setf (aref result i) (aref vector i))
    (finally (return result))))

(defun pkcs7-unpad (vector)
  (declare (type (simple-array (unsigned-byte 8) (*)) vector))
  (make-array (- (length vector) (last-elt vector))
              :element-type '(unsigned-byte 8)
              :displaced-to vector
              :displaced-index-offset 0))
