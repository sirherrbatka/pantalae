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


(defgeneric sending-chain (ratchet))
(defgeneric receiving-chain (ratchet))
(defgeneric kdf (chain))
(defgeneric forward (chain bytes &optional length))
(defgeneric steps (chain))
(defgeneric (setf steps) (new-value chain))
(defgeneric key (chain))
(defgeneric (setf key) (new-value chain))
(defgeneric move-ratchet (ratchet &optional bytes))
(defgeneric next-key (ratchet &optional bytes))
(defgeneric constant (symmetric-key-ratchet))
(defgeneric symetric-key-ratchet (diffie-hellman-ratchet))
(defgeneric private-key (diffie-hellman-ratchet))
(defgeneric public-key (diffie-hellman-ratchet))
(defgeneric derive-public (diffie-hellman-ratchet))
(defgeneric derive-private (diffie-hellman-ratchet))
(defgeneric long-term-identity-key (client))
(defgeneric signed-pre-key (client))
(defgeneric exchange-keys* (this-client other-client))
(defgeneric encrypt* (this-client other-client message start end result))
(defgeneric decrypt* (this-client other-client cipher start end result))
(defgeneric rotate-ratchet (this-client public-key))
(defgeneric encrypt (double-ratchet message start end &optional result))
(defgeneric decrypt (double-ratchet cippher key start end &optional result))
(defgeneric long-term-identity-remote-key (double-ratchet))
