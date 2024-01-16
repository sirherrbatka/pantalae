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

(defmethod extended-triple-diffie-hellman* ((client-a client)
                                            (client-b client))
  (let* ((dh1 (exchange-25519-key (private (signed-pre-key client-a))
                                  (public (long-term-identity-key client-b))))
         (dh2 (exchange-25519-key (private (long-term-identity-key client-a))
                                  (public (ephemeral-key client-b))))
         (dh3 (exchange-25519-key (private (signed-pre-key client-a))
                                  (public (ephemeral-key client-b))))
         (dh4 (exchange-25519-key (private (one-time-pre-keys client-a))
                                  (public (ephemeral-key client-b)))))
    (setf (slot-value client-a '%shared-key) (concatenate '(simple-array (unsigned-byte 8) (*))
                                                          dh1 dh2 dh3 dh4)
          (slot-value client-b '%shared-key) (slot-value client-a '%shared-key))))

(defmethod private-key ((ratchet diffie-hellman-ratchet))
  (private (keys ratchet)))

(defmethod public-key ((ratchet diffie-hellman-ratchet))
  (public (keys ratchet)))

(defmethod encrypt* ((this-client client)
                     (other-client client)
                     message)
  (rotate-ratchet this-client (public (keys other-client)))
  (multiple-value-bind (key iv)
      (~> this-client
          diffie-hellman-ratchet
          sending-ratchet
          next-key)
    (lret ((result (copy-array message)))
      (ic:encrypt (ic:make-cipher :aes :key key :mode :cbc :initialization-vector iv)
                  message
                  result))))

(defmethod decrypt* ((this-client client)
                     (other-client client)
                     cipher)
  (rotate-ratchet this-client (public (keys other-client)))
  (multiple-value-bind (key iv)
      (~> this-client
          diffie-hellman-ratchet
          receiving-ratchet
          next-key)
    (lret ((result (copy-array cipher)))
      (ic:decrypt (ic:make-cipher :aes :key key :mode :cbc :initialization-vector iv)
                  cipher
                  result))))

(defmethod rotate-ratchet ((this-client client) public-key)
  (unless (null (keys this-client))
    (let* ((dh-recv (ic:diffie-hellman (private (keys this-client)) public-key))
           (shared-recv (~> this-client diffie-hellman-ratchet root-ratchet (next-key dh-recv))))
      (setf (receiving-ratchet (diffie-hellman-ratchet this-client)) (make-symmetric-ratchet shared-recv))))
  (setf (keys this-client) (make-25519-keys))
  (let* ((dh-send (ic:diffie-hellman (private (keys this-client)) public-key))
         (shared-send (~> this-client diffie-hellman-ratchet root-ratchet (next-key dh-send))))
    (setf (sending-ratchet (diffie-hellman-ratchet this-client)) (make-symmetric-ratchet shared-send))))

(defmethod encrypt ((double-ratchet double-ratchet)
                    message)
  (encrypt* (local-client double-ratchet)
            (remote-client double-ratchet)
            message))

(defmethod decrypt ((double-ratchet double-ratchet)
                    cipher)
  (decrypt* (local-client double-ratchet)
            (remote-client double-ratchet)
            cipher))
