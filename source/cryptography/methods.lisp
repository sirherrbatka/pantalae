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


(defmethod print-object ((object keys-pair) stream)
  (print-unreadable-object (object stream)
    (format stream "public: ~a private: ~a" (public object) (private object))))

(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream)
    (format stream "ID: ~a EPH: ~a" (long-term-identity-key object) (ephemeral-key object))))

(defmethod forward ((chain chain) bytes &optional (length 32))
  (let* ((chain-key-length (chain-key-length chain))
         (output (ic:derive-key (kdf chain)
                                bytes
                                (key chain)
                                (iteration-count chain)
                                (+ chain-key-length length)))
         (result (subseq output chain-key-length)))
    (setf (key chain) (subseq output 0 chain-key-length))
    (write-steps (1+ (steps chain)) chain )
    result))

(defmethod steps ((ratchet symmetric-ratchet))
  (steps (chain ratchet)))

(defmethod move-ratchet ((ratchet symmetric-ratchet) &optional (bytes (constant ratchet)))
  (forward (chain ratchet) bytes 80))

(defmethod next-key ((ratchet symmetric-ratchet) &optional (bytes (constant ratchet)))
  (let ((result (move-ratchet ratchet bytes)))
    (values (subseq result 32 64)
            (subseq result 64))))

(defmethod exchange-keys* ((client-a local-client)
                           (client-b remote-client))
  (if (iterate
        (for a in-vector (~> client-a long-term-identity-key public ironclad:curve25519-key-y))
        (for b in-vector (~> client-b long-term-identity-key public ironclad:curve25519-key-y))
        (finding t such-that (> a b))
        (finding nil such-that (< a b)))
      (let* ((dh1 (exchange-25519-key (~> client-a long-term-identity-key private)
                                      (~> client-b ephemeral-key-1 public)))
             (dh2 (exchange-25519-key (~> client-a ephemeral-key-2 private)
                                      (~> client-b long-term-identity-key public)))
             (dh3 (exchange-25519-key (~> client-a ephemeral-key-3 private)
                                      (~> client-b long-term-identity-key public)))
             (dh4 (exchange-25519-key (~> client-a ephemeral-key-4 private)
                                      (~> client-b long-term-identity-key public))))
        (setf (slot-value client-a '%shared-key) (concatenate '(simple-array (unsigned-byte 8) (*)) dh1 dh2 dh3 dh4)
              (slot-value client-b '%shared-key) (slot-value client-a '%shared-key)))
      (let* ((dh1 (exchange-25519-key (~> client-a ephemeral-key-1 private)
                                      (~> client-b long-term-identity-key public)))
             (dh2 (exchange-25519-key (~> client-a long-term-identity-key private)
                                      (~> client-b ephemeral-key-2 public)))
             (dh3 (exchange-25519-key (~> client-a long-term-identity-key  private)
                                      (~> client-b ephemeral-key-3 public)))
             (dh4 (exchange-25519-key (~> client-a long-term-identity-key private)
                                      (~> client-b ephemeral-key-4 public))))
        (setf (slot-value client-a '%shared-key) (concatenate '(simple-array (unsigned-byte 8) (*))
                                                              dh1 dh2 dh3 dh4)
              (slot-value client-b '%shared-key) (slot-value client-a '%shared-key))))
  nil)

(defmethod private-key ((ratchet diffie-hellman-ratchet))
  (private (keys ratchet)))

(defmethod public-key ((ratchet diffie-hellman-ratchet))
  (public (keys ratchet)))

(defmethod encrypt* ((this-client client)
                     (other-client client)
                     message
                     start
                     end
                     result)
  (multiple-value-bind (key iv)
      (~> this-client
          diffie-hellman-ratchet
          sending-ratchet
          next-key)
    (ic:encrypt (ic:make-cipher :aes :key key :mode :cbc :initialization-vector iv)
                message
                result
                :ciphertext-end end
                :ciphertext-start start
                :plaintext-start start
                :plaintext-end end)
    result))

(defmethod decrypt* ((this-client client)
                     (other-client client)
                     cipher
                     start
                     end
                     result)
  (multiple-value-bind (key iv)
      (~> this-client
          diffie-hellman-ratchet
          receiving-ratchet
          next-key)
    (ic:decrypt (ic:make-cipher :aes :key key :mode :cbc :initialization-vector iv)
                cipher
                result
                :plaintext-start start
                :plaintext-end end
                :ciphertext-start start
                :ciphertext-end end)
    result))

(defmethod rotate-ratchet ((this-client client) public-key)
  (when (null (keys this-client))
    (let* ((dh-recv (ic:diffie-hellman (private (keys this-client)) public-key))
           (shared-recv (~> this-client diffie-hellman-ratchet root-ratchet (next-key dh-recv))))
      (setf (receiving-ratchet (diffie-hellman-ratchet this-client)) (make-symmetric-ratchet shared-recv))))
  (setf (keys this-client) (make-25519-keys))
  (let* ((dh-send (ic:diffie-hellman (private (keys this-client)) public-key))
         (shared-send (~> this-client diffie-hellman-ratchet root-ratchet (next-key dh-send))))
    (setf (sending-ratchet (diffie-hellman-ratchet this-client)) (make-symmetric-ratchet shared-send))))

(defmethod encrypt ((double-ratchet double-ratchet)
                    message
                    start
                    end
                    &optional (result (make-array (array-dimensions message) :element-type '(unsigned-byte 8))))
  (bt:with-lock-held ((lock double-ratchet))
    (let ((result (encrypt* (local-client double-ratchet)
                            (remote-client double-ratchet)
                            message
                            start
                            end
                            result)))
      (list
       (public (keys (local-client double-ratchet)))
       result))))

(defmethod decrypt ((double-ratchet double-ratchet)
                    cipher
                    key
                    start
                    end
                    &optional (result (make-array (array-dimensions cipher) :element-type '(unsigned-byte 8))))
  (bt:with-lock-held ((lock double-ratchet))
    (setf (public (keys (remote-client double-ratchet))) key)
    (rotate-ratchet (local-client double-ratchet) key)
    (decrypt* (local-client double-ratchet)
              (remote-client double-ratchet)
              cipher
              start
              end
              result)))

(defmethod long-term-identity-remote-key ((object double-ratchet))
  (~> object remote-client long-term-identity-key public))
