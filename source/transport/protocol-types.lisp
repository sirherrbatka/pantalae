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

(defclass routing-table ()
  ((%own-routes
    :initarg :own-routes
    :reader own-routes))
  (:default-initargs
   :own-routes (make-hash-table :test 'equalp)))

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
    :accessor long-term-identity-key))
  (:default-initargs
   :message-table (make 'message-table)
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

(defclass message (pantalea.utils.dependency:dependency-cell)
  ((%hop-counter
    :initarg :hop-counter
    :accessor hop-counter)
   (%id
    :initarg :id
    :reader id)
   (%origin-public-key
    :initarg :origin-public-key
    :reader origin-public-key)
   (%destination
    :initarg :destination
    :accessor destination))
  (:default-initargs
   :hop-counter 0
   :destination nil
   :id (random most-positive-fixnum)))

(defclass peer-discovery-request (message)
  ())

(defclass response ()
  ((%id
    :initarg :id
    :reader id)
   (%encrypted-payload
    :initarg :encrypted-payload
    :reader encrypted-payload)))

(defclass response-payload ()
  ((%origin-public-key
    :initarg :origin-public-key
    :reader origin-public-key)))

(defclass peer-discovery-payload (response-payload)
  ((%destination
    :initarg :destination
    :reader destination)))

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
   (%origin-public-key
    :reader origin-public-key
    :initarg :origin-public-key)
   (%nest
    :reader read-nest
    :initarg :nest)
   (%connection
    :reader connection
    :initarg :connection)))

(defclass peer-discovery-handler (message-handler)
  ())
