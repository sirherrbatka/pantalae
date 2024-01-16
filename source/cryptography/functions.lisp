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


(defun keys-load (source)
  (let ((result
          (if (streamp source)
              (conspack:decode-stream source)
              (conspack:decode-file source))))
    (check-type result public-key)
    result))

(defun keys-save (destination keys)
  (check-type keys public-key)
  (if (streamp destination)
      (conspack:encode keys :stream destination)
      (conspack:encode-to-file keys destination))
  keys)

(defun make-chain (key)
  (make-instance 'chain :key key))

(defun make-symmetric-ratchet (key)
  (make-instance 'symmetric-ratchet :chain (make-chain key)))

(defun make-diffie-hellman-ratchet (shared-key &optional (root-ratchet (make-symmetric-ratchet shared-key)))
  (let* ((recv-chain (make-symmetric-ratchet (next-key root-ratchet)))
         (send-ratchet (make-symmetric-ratchet (next-key root-ratchet))))
    (make-instance 'diffie-hellman-ratchet
                   :root-ratchet root-ratchet
                   :receiving-ratchet recv-chain
                   :sending-ratchet send-ratchet)))

(defun extended-triple-diffie-hellman (this-client other-client)
  (extended-triple-diffie-hellman* this-client other-client)
  (setf (slot-value this-client '%diffie-hellman-ratchet) (make-diffie-hellman-ratchet (shared-key this-client))
        (slot-value other-client '%diffie-hellman-ratchet) (make-diffie-hellman-ratchet (shared-key other-client)))
  nil)

(defun make-double-ratchet (local-client remote-client)
  (check-type local-client local-client)
  (check-type remote-client remote-client)
  (extended-triple-diffie-hellman *local-client* *remote-client*)
  (make-instance 'double-ratchet
                  :local-client local-client
                  :remote-client remote-client))
