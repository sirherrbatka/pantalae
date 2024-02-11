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


(defclass public-key ()
  ((%public :initarg :public
            :initform nil
            :accessor public)))

(defclass keys-pair (public-key)
  ((%private :initarg :private
             :initform nil
             :accessor private)))

(conspack:defencoding ic:curve25519-public-key
  ic::y)

(conspack:defencoding ic:curve25519-private-key
  ic::x ic::y)

(conspack:defencoding public-key
  %public)

(conspack:defencoding keys-pair
  %private)

(defclass ratchet ()
  ((%root-key
    :initarg :root-key
    :accessor root-key
    :accessor rk)
   (%send-keys
    :initform nil
    :initarg :send-keys
    :accessor send-keys)
   (%receive-key
    :initarg :receive-key
    :initform nil
    :accessor receive-key)
   (%chain-key-receive
    :initarg :chain-key-receive
    :initform nil
    :accessor ckr
    :accessor chain-key-receive)
   (%chain-key-send
    :initarg :chain-key-send
    :initform nil
    :accessor cks
    :accessor chain-key-send)
   (%number-of-sent-messages
    :initarg :number-of-sent-messages
    :initform nil
    :accessor number-of-sent-messages)
   (%number-of-received-messages
    :accessor number-of-received-messages
    :initarg :number-of-received-messages)
   (%number-of-messages-in-previous-sending-chain
    :accessor number-of-messages-in-previous-sending-chain
    :initarg :number-of-messages-in-previous-sending-chain
    :initform 0)
   (%kdf
    :initarg :kdf
    :reader kdf)
   (%constant
    :initarg :constant
    :reader constant))
  (:default-initargs
   :number-of-sent-messages 0
   :number-of-received-messages 0
   :root-key nil
   :constant (ic:make-random-salt 0)
   :kdf (ic:make-kdf :hmac-kdf :digest :sha256)))

(defclass client ()
  ((%long-term-identity-key :initarg :long-term-identity-key
                            :reader long-term-identity-key)
   (%ephemeral-key-1 :initarg :ephemeral-key-1
                     :accessor ephemeral-key-1)
   (%ephemeral-key-2 :initarg :ephemeral-key-2
                     :accessor ephemeral-key-2)
   (%ephemeral-key-3 :initarg :ephemeral-key-3
                     :accessor ephemeral-key-3)
   (%ephemeral-key-4 :initarg :ephemeral-key-4
                     :accessor ephemeral-key-4)
   (%shared-key :initarg :shared-key
                :reader shared-key)
   (%keys :initarg :keys
          :accessor keys)
   (%ratchet :initarg :ratchet
             :accessor ratchet))
  (:default-initargs
   :keys (make-25519-keys)
   :ratchet nil
   :ephemeral-key-1 (make-25519-keys)
   :ephemeral-key-2 (make-25519-keys)
   :ephemeral-key-3 (make-25519-keys)
   :ephemeral-key-4 (make-25519-keys)
   :long-term-identity-key (make-25519-keys)))

(defclass remote-client (client)
  ())

(defclass local-client (client)
  ())

(defclass session ()
  ((%this-client :initarg :this-client
                 :reader this-client)
   (%other-client :initarg :other-client
                  :reader other-client)))

(defclass double-ratchet ()
  ((%lock
    :initarg :lock
    :reader lock)
   (%local-client
    :initarg :local-client
    :accessor local-client)
   (%remote-client
    :initarg :remote-client
    :accessor remote-client))
  (:default-initargs
   :lock (bt2:make-lock)))
