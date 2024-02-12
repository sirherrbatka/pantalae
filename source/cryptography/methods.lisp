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
    (format stream "ID: ~a" (long-term-identity-key object))))

(defmethod print-object ((object ratchet) stream)
  (print-unreadable-object (object stream)
    (format stream "RK: ~a CKS: ~a CKR: ~A" (rk object) (cks object) (ckr object))))

(defmethod exchange-keys* ((client-a local-client)
                           (client-b client))
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
              (slot-value client-b '%shared-key) (slot-value client-a '%shared-key)
              (ratchet client-a) (bind ((sk (slot-value client-a '%shared-key)) ; this is remote-client RK
                                        ((:values rk cks)
                                         (kdf-rk sk (exchange-25519-key (~> client-a ephemeral-key-1 private)
                                                                            (~> client-b ephemeral-key-1 public)))))
                                   (make 'ratchet
                                         :root-key rk
                                         :chain-key-send cks
                                         :send-keys (ephemeral-key-1 client-a)
                                         :receive-key (~> client-b ephemeral-key-1 public ic:curve25519-key-y)
                                         :chain-key-receive nil))))
      (let* ((dh1 (exchange-25519-key (~> client-a ephemeral-key-1 private)
                                      (~> client-b long-term-identity-key public)))
             (dh2 (exchange-25519-key (~> client-a long-term-identity-key private)
                                      (~> client-b ephemeral-key-2 public)))
             (dh3 (exchange-25519-key (~> client-a long-term-identity-key  private)
                                      (~> client-b ephemeral-key-3 public)))
             (dh4 (exchange-25519-key (~> client-a long-term-identity-key private)
                                      (~> client-b ephemeral-key-4 public))))
        (setf (slot-value client-a '%shared-key) (concatenate '(simple-array (unsigned-byte 8) (*)) dh1 dh2 dh3 dh4)
              (slot-value client-b '%shared-key) (slot-value client-a '%shared-key)
              (ratchet client-a) (make 'ratchet
                                       :root-key (slot-value client-a '%shared-key)
                                       :send-keys (~> client-a ephemeral-key-1)))))

  nil)

(defmethod encrypt* ((this-client client)
                     (other-client client)
                     message
                     result)
  (bind ((ratchet (ratchet this-client))
         ((:values chain-key message-key initialization-vector) (kdf-ck ratchet (cks ratchet))))
    (incf (number-of-sent-messages ratchet))
    (setf (cks ratchet) chain-key)
    (ic:encrypt (ic:make-cipher :aes
                                :key message-key
                                :mode :cbc
                                :padding :pkcs7
                                :initialization-vector initialization-vector)
                (pkcs7-pad message)
                result)
    result))

(defmethod decrypt* ((this-client client)
                     (other-client client)
                     ciphertext
                     result)
  (bind ((ratchet (ratchet this-client))
         ((:values chain-key message-key iv) (kdf-ck ratchet (ckr ratchet))))
    (setf (ckr ratchet) chain-key)
    (incf (number-of-received-messages ratchet))
    (ic:decrypt (ic:make-cipher :aes
                                :key message-key
                                :mode :cbc
                                :padding :pkcs7
                                :initialization-vector iv)
                ciphertext
                result)
    (pkcs7-unpad result)))

(defmethod dh-ratchet ((this-client client)
                       public-key
                       number-of-sent-messages
                       number-of-messages-in-previous-sending-chain)
  (let ((ratchet (ratchet this-client)))
    (when (and (not (null (receive-key ratchet)))
               (vector= (ironclad:curve25519-key-y (receive-key ratchet))
                        (ironclad:curve25519-key-y public-key)))
      (return-from dh-ratchet nil))
    (shiftf (number-of-messages-in-previous-sending-chain ratchet)
            (number-of-sent-messages ratchet)
            0)
    (bind (((:values rk ckr)
            (kdf-rk (rk ratchet)
                    (exchange-25519-key (~> ratchet send-keys private)
                                        public-key))))
      (setf (receive-key ratchet) public-key
            (root-key ratchet) rk
            (ckr ratchet) ckr
            (send-keys ratchet) (make-25519-keys)))
    (bind (((:values rk cks)
            (kdf-rk (rk ratchet)
                    (exchange-25519-key (~> ratchet send-keys private)
                                        (receive-key ratchet)))))
      (setf (root-key ratchet) rk
            (cks ratchet) cks)))
  nil)

(defmethod encrypt ((double-ratchet double-ratchet)
                    message
                    &optional (result (make-padded-vector message)))
  (bt2:with-lock-held ((lock double-ratchet))
    (let ((result (encrypt* (local-client double-ratchet)
                            (remote-client double-ratchet)
                            message
                            result)))
      (list result
            (~> double-ratchet local-client ratchet send-keys public)
            (~> double-ratchet local-client ratchet number-of-sent-messages)
            (~> double-ratchet local-client ratchet number-of-messages-in-previous-sending-chain)))))

(defmethod decrypt ((double-ratchet double-ratchet)
                    data
                    &optional (result (make-array (array-dimensions (first data)) :element-type '(unsigned-byte 8))))
  (bind (((ciphertext send-key number-of-sent-messages number-of-messages-in-previous-sending-chain) data))
    (bt2:with-lock-held ((lock double-ratchet))
      (dh-ratchet (local-client double-ratchet)
                  send-key
                  number-of-sent-messages
                  number-of-messages-in-previous-sending-chain)
      (decrypt* (local-client double-ratchet)
                (remote-client double-ratchet)
                ciphertext
                result))))

(defmethod long-term-identity-remote-key ((object double-ratchet))
  (~> object remote-client long-term-identity-key public))

(defmethod can-encrypt-p ((object double-ratchet))
  (~> object local-client can-encrypt-p))

(defmethod can-encrypt-p ((object client))
  (~> object ratchet cks null not))
