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

(conspack:defencoding public-key
  %public)

(conspack:defencoding keys-pair
  %private)


(defclass chain ()
  ((%kdf :initarg :kdf
         :reader kdf)
   (%key :initarg :key
         :accessor key)
   (%steps :initarg :steps
           :reader steps
           :writer write-steps)
   (%iteration-ocunt :initarg :iteration-count
                     :reader iteration-count))
  (:default-initargs
   :steps 0
   :iteration-count 4
   :kdf (ironclad:make-kdf :hmac-kdf :digest :tree-hash)
   :key (ironclad:make-random-salt)))

(defclass symmetric-ratchet ()
  ((%chain :initarg :chain
           :reader chain)
   (%constant :initarg :constant
              :reader constant))
  (:default-initargs
   :constant (ironclad:make-random-salt 0)))

(defclass diffie-hellman-ratchet ()
  ((%root-ratchet :initarg :root-ratchet
                  :reader root-ratchet)
   (%receiving-ratchet :initarg :receiving-ratchet
                       :accessor receiving-ratchet)
   (%sending-ratchet :initarg :sending-ratchet
                     :accessor sending-ratchet)))

(defclass client ()
  ((%long-term-identity-key :initarg :long-term-identity-key
                            :reader long-term-identity-key)
   (%shared-key :initarg :shared-key
                :reader shared-key)
   (%keys :initarg :keys
          :accessor keys)
   (%diffie-hellman-ratchet :initarg :diffie-hellman-ratchet
                            :accessor diffie-hellman-ratchet))
  (:default-initargs
   :keys nil
   :long-term-identity-key (make-25519-keys)))

(defclass remote-client (client)
  ((%signed-pre-key :initarg :signed-pre-key
                    :reader signed-pre-key)
   (%one-time-pre-keys :initarg :one-time-pre-keys
                       :accessor one-time-pre-keys))
  (:default-initargs
   :keys (make-25519-keys)
   :signed-pre-key (make-25519-keys)
   :one-time-pre-keys (make-25519-keys)))

(defclass local-client (client)
  ((%ephemeral-key :initarg :ephemeral-key
                   :accessor ephemeral-key))
  (:default-initargs
   :ephemeral-key (make-25519-keys)))

(defclass session ()
  ((%this-client :initarg :this-client
                 :reader this-client)
   (%other-client :initarg :other-client
                  :reader other-client)))
