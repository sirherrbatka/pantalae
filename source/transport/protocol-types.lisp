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


(defclass routing-table ()
  ((%own-routes
    :initarg :own-routes
    :reader own-routes))
  (:default-initargs
   :own-routes (make-hash-table :test 'equalp)))

(defclass fundamental-nest ()
  ((%peers
    :initarg :peers
    :accessor peers)
   (%routing-table
    :initarg :routing-table
    :reader routing-table)
   (%main-nest-lock
    :initarg :main-nest-lock
    :reader main-nest-lock)
   (%long-term-identity-key
    :initarg :long-term-identity-key
    :accessor long-term-identity-key))
  (:default-initargs
   :routing-table (make 'routing-table)
   :main-nest-lock (bt:make-lock "NEST lock")))

(defclass route-container (pantalea.utils.dependency:dependency-cell)
  ((%content
    :initarg :content
    :accessor content)))

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

(defclass fundamental-gossip ()
  ((%day                                ; 32 bits
    :initarg :day
    :accessor day)
   (%sec                                ; 32 bits
    :initarg :sec
    :accessor sec)
   (%nsec                               ; 32 bits
    :initarg :nsec
    :accessor nsec)
   (%id                                 ; 32 bits
    :initarg :id
    :reader gossip-id
    :accessor id)
   (%payload
    :initarg :payload
    :reader gossip-payload
    :accessor payload))
  (:default-initargs
   :id (random most-positive-fixnum)
   :forward-route '()
   :backward-route '()))

(defclass route-discovery ()
  ((%destination-public-key
    :initarg :destination-public-key
    :accessor destination-public-key
    :reader route-discovery-destination-public-key)
   (%forward-route
    :initarg :forward-route
    :reader route-discovery-forward-route
    :accessor forward-route)
   (%backward-route
    :initarg :backward-route
    :reader route-discovery-backward-route
    :accessor backward-route)
   (%palyoad
    :initarg :palyoad
    :reader route-discovery-payload
    :accessor palyoad)))

(defclass fundamental-payload ()
  ())

(defclass channels-group ()
  ((%channels
    :initarg :channels
    :reader channels-group-channels)
   (%nest
    :initarg :nest
    :reader channels-group-nest))
  (:default-initargs
   :channels (make-hash-table)))

(defclass fundamental-channel ()
  ((%group
    :initarg :group
    :reader channel-group)
   (%port-number
    :initarg :port-number
    :reader channel-port-number)
   (%nest
    :initarg :nest
    :reader channel-nest)
   (%peer
    :initarg :peer
    :reader channel-peer)))

(defclass enveloped-gossip (fundamental-gossip)
  ((%destination-public-key
    :initarg :destination-public-key
    :reader enveloped-gossip-destination-public-key
    :accessor destination-public-key))
  (:default-initargs))

(defclass opened-gossip (fundamental-gossip)
  ()
  (:default-initargs))

(defclass fundamental-network-destination ()
  ((%peer :initarg :peer
          :reader network-destination-peer)))

(define-condition nest-stopped (error)
  ())

(define-condition nest-started (error)
  ())

(deftype packet ()
  `(simple-array (*) (unsigned-byte 8)))
