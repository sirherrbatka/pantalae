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
(cl:defpackage #:pantalea.transport.protocol
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:dr #:pantalea.cryptography)
   (#:promise #:pantalea.utils.promise)
   (#:bloom #:pantalea.utils.bloom-filter)
   (#:q #:pantalea.utils.queue)
   (#:tw #:pantalea.utils.timing-wheel)
   (#:dr #:pantalea.cryptography))
  (:export
   #:+type-data+
   #:+type-echo+
   #:+type-gossip+
   #:+type-keys+
   #:+type-message+
   #:+type-ping+
   #:+type-pong+
   #:+type-response+
   #:+type-route-discovery+
   #:+type-cryptographic-handshake+
   #:connect
   #:connected
   #:decrypt
   #:destination
   #:destination-key
   #:destination-public-key
   #:disconnect
   #:disconnected
   #:double-ratchet
   #:encrypt
   #:failed-to-connect
   #:forget-message
   #:fundamental-connection
   #:fundamental-network-destination
   #:handle-incoming-message
   #:handle-incoming-packet
   #:handle-incoming-response
   #:hop-counter
   #:long-term-identity-key
   #:make-double-ratchet-local-client
   #:map-connections
   #:nest
   #:networkind-symbol
   #:nest-started
   #:nest-stopped
   #:network-destination-peer
   #:networking-of-type
   #:origin-public-key
   #:packet
   #:peer-discovery-request
   #:peer-discovery-response
   #:per-discovery-response
   #:send-echo
   #:add-networking
   #:networking-symbol
   #:open-channel
   #:discover-peers
   #:ping-at
   #:pong-at
   #:schedule-to-event-loop
   #:schedule-to-event-loop/no-lock
   #:send-keys
   #:send-message
   #:send-packet
   #:send-response
   #:set-double-ratchet
   #:spread-message
   #:validate-connection-encryption
   #:start-nest
   #:start-networking
   #:stop-nest
   #:stop-networking
   #:with-main-lock-held))

(cl:defpackage #:pantalea.transport.intra
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:p #:pantalea.transport.protocol)
   (#:dr #:pantalea.cryptography)
   (#:q #:pantalea.utils.queue)
   (#:promise #:pantalea.utils.promise))
  (:export
   #:*nest-map*
   #:destination
   #:networking))

(cl:defpackage #:pantalea.transport.tcp
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:dr #:pantalea.cryptography)
   (#:p #:pantalea.transport.protocol)
   (#:promise #:pantalea.utils.promise))
  (:export
   #:ip-destination
   #:networking))
