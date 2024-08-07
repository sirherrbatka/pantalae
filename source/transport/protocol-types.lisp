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
(cl:in-package #:pantalea.transport.protocol)


(defclass fundamental-network-destination ()
  ())

(defun key-hash (key)
  (~> key pantalea.utils.hashing:hash-key
      (ldb (byte 0 #.(integer-length most-positive-fixnum)) _)))

(defun key-equal (a b)
  (vector= (ironclad:curve25519-key-y a)
           (ironclad:curve25519-key-y b)))

(cl-custom-hash-table:define-custom-hash-table-constructor make-key-table
  :test key-equal :hash-function key-hash)

(defclass routing-table ()
  ((%own-routes
    :initarg :own-routes
    :reader own-routes))
  (:default-initargs
   :own-routes (make-key-table)))

(defclass message-table ()
  ((%active-messages
    :initarg :active-messages
    :reader active-messages))
  (:default-initargs
   :active-messages (make-hash-table)))

(defclass nest ()
  ((%networking
    :initarg :networking
    :accessor networking)
   (%timing-wheel
    :initarg :timing-wheel
    :accessor timing-wheel)
   (%event-loop-queue
    :initarg :event-loop-queue
    :accessor event-loop-queue)
   (%event-loop-thread
    :initarg :event-loop-thread
    :accessor event-loop-thread)
   (%message-table
    :initarg :message-table
    :accessor message-table)
   (%started
    :initarg :started
    :accessor started)
   (%main-nest-lock
    :initarg :main-nest-lock
    :reader main-nest-lock)
   (%routing-table
    :initarg :routing-table
    :accessor routing-table)
   (%long-term-identity-key
    :initarg :long-term-identity-key
    :accessor long-term-identity-key)
   (%maximum-connections-count
    :initarg :maximum-connections-count
    :accessor maximum-connections-count))
  (:default-initargs
   :message-table (make 'message-table)
   :maximum-connections-count 16
   :started nil
   :timing-wheel nil
   :event-loop-thread nil
   :networking (make-hash-table)
   :event-loop-queue (q:make-blocking-queue)
   :routing-table (make 'routing-table)
   :main-nest-lock (bt2:make-lock :name "NEST lock")))

(defclass route-container (pantalea.utils.dependency:dependency-cell)
  ((%content
    :initarg :content
    :accessor content)
   (%routing-table
    :initarg :routing-table
    :accessor routing-table)))

(defclass pending-route ()
  ((%destination-public-key
    :initarg :destination-public-key
    :reader destination-public-key)
   (%established-promise
    :initarg :on-established
    :reader on-established)
   (%timeout-promise
    :initarg :timeout
    :reader timeout-promise)))

(defclass established-route (pantalea.utils.dependency:dependency-cell)
  ((%connection
    :initarg :connection
    :reader connection)))

(defclass dead-own-route (pantalea.utils.dependency:dependency-cell)
  ((%destination-public-key
     :initarg :destination-public-key
     :reader destination-public-key)))

(defclass own-route (established-route)
  ((%destination-public-key
    :initarg :destination-public-key
    :reader destination-public-key)))

(defclass direct-route (own-route)
  ())

(defclass indirect-route (own-route)
  ((%route-id
    :initarg :route-id
    :reader route-id)))

(defclass forwarding (established-route)
  ((%route-id
    :initarg :route
    :reader route-id)))

(defclass fundamental-connection (pantalea.utils.dependency:dependency-cell)
  ((%ping-at
    :initarg :ping-at
    :accessor ping-at)
   (%pong-at
    :initarg :pong-at
    :accessor pong-at)
   (%destination
    :initarg :destination
    :accessor destination)
   (%pong-timeout-promise
    :initarg :pong-timeout-promise
    :accessor pong-timeout-promise)
   (%double-ratchet
    :initarg :double-ratchet
    :accessor double-ratchet))
  (:default-initargs
   :ping-at nil
   :double-ratchet nil
   :pong-timeout-promise nil
   :pong-at nil))

(defclass dead-connection (fundamental-connection)
  ())

(defclass message ()
  ((%hop-counter
    :initarg :hop-counter
    :accessor hop-counter)
   (%id
    :initarg :id
    :reader id)
   (%destination
    :initarg :destination
    :accessor destination))
  (:default-initargs
   :hop-counter 0
   :destination nil
   :id (random most-positive-fixnum)))

(defclass envelop ()
  ((%ephemeral-key
    :initarg :ephemeral-key
    :reader ephemeral-key)
   (%nonce
    :initarg :nonce
    :accessor nonce)
   (%encrypted
    :initarg :encrypted
    :accessor encrypted)))

(defclass shroud ()
  ((%nonce
    :initarg :nonce
    :accessor nonce)
   (%encrypted
    :initarg :encrypted
    :accessor encrypted)))

(defclass enveloped-message (envelop message)
  ())

(defclass public-request (message)
  ((%origin-public-key :initarg :origin-public-key
                       :reader read-origin-public-key)))

(defclass peer-discovery-request (public-request)
  ())

(defclass route-discovery-request (envelop message)
  ())

(defclass fundamental-response ()
  ((%id
    :initarg :id
    :reader id)
   (%destination
    :initarg :destination
    :accessor destination))
  (:default-initargs :destination nil))

(defclass payload-response (envelop fundamental-response)
  ((%encrypted-payload
    :initarg :encrypted-payload
    :reader encrypted-payload)))

(defclass response-payload ()
  ())

(defclass peer-discovery-payload (response-payload)
  ((%connected-peers
    :initarg :connected-peers
    :reader connected-peers)))

(defclass route-discovery-response (shroud fundamental-response)
  ())

(defclass fundamental-payload ()
  ())

(define-condition nest-stopped (error)
  ())

(define-condition nest-started (error)
  ())

(deftype packet ()
  `(simple-array (*) (unsigned-byte 8)))

(defclass message-handler (pantalea.utils.dependency:dependency-cell)
  ((%id
    :reader id
    :initarg :id)
   (%nest
    :reader read-nest
    :initarg :nest)
   (%connection
    :reader connection
    :initarg :connection)))

(defclass peer-discovery-handler (message-handler)
  ((%responses
    :initarg :responses
    :accessor responses
    :initform '())))

(defclass route-discovery-handler (message-handler)
  ((%responses
    :initarg :responses
    :accessor responses
    :initform '())))

(defclass foreign-route-discovery-handler (message-handler)
  ())
