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


(defclass peer ()
  ((%public-key
    :initarg :public-key
    :reader peer-public-key
    :accessor public-key)
   (%prefered-route
    :initarg :prefered-route
    :accessor prefered-route)
   (%routes
    :initarg :routes
    :reader peer-routes
    :accessor routes))
  (:default-initargs
   :prefered-route nil
   :routes nil))

(defclass fundamental-nest ()
  ((%channels
    :initarg :channels
    :accessor channels)
   (%connections
    :initarg :connections
    :accessor connections)
   (%peers
    :initarg :peers
    :accessor peers)
   (%public-key
    :initarg :public-key
    :accessor public-key)
   (%private-key
    :initarg :private-key
    :accessor private-key)))

(defclass fundamental-message ()
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
    :reader message-id
    :accessor id)
   (%forward-route
    :initarg :forward-route
    :reader message-forward-route
    :accessor forward-route)
   (%backward-route
    :initarg :backward-route
    :reader message-backward-route
    :accessor backward-route)
   (%payload
    :initarg :payload
    :reader message-payload
    :accessor payload))
  (:default-initargs
   :id (random most-positive-fixnum)
   :forward-route '()
   :backward-route '()))

(defclass fundamental-payload ()
  ())

(defclass connection-request-payload (fundamental-payload)
  ((%id
    :initarg :id
    :accessor id)
   (%salt
    :initarg :salt
    :accessor salt)
   (%service-name
    :initarg :service-name
    :accessor service-name)
   (%public-key
    :initarg :public-key
    :accessor public-key))
  (:default-initargs
   :id (random most-positive-fixnum)
   :salt (ironclad:make-random-salt)))

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

(defclass enveloped-message (fundamental-message)
  ((%destination-public-key
    :initarg :destination-public-key
    :reader enveloped-message-destination-public-key
    :accessor destination-public-key))
  (:default-initargs))

(defclass opened-message (fundamental-message)
  ()
  (:default-initargs))
