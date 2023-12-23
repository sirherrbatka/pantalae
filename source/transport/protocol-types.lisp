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


(defclass fundamental-message ()
  ((%day ; 32 bits
    :initarg :day
    :accessor day)
   (%sec ; 32 bits
    :initarg :sec
    :accessor sec)
   (%nsec ; 32 bits
    :initarg :nsec
    :accessor nsec)
   (%id ; 32 bits
    :initarg :id
    :reader message-id
    :accessor id)
   (%payload
    :initarg :payload
    :reader message-payload
    :accessor payload))
  (:default-initargs
   :id (random most-positive-fixnum)))

(defclass channel-group ()
  ((%channels
    :initarg :channels
    :accessor channels)))

(defclass channel ()
  ((%group
    :initarg :group
    :reader channel-group)
   (%service-name
    :initarg :service-name
    :reader channel-service-name)
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
