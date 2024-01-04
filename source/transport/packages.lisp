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
  (:local-nicknames)
  (:export
   #:gossip-timestamp
   #:gossip-id
   #:enveloped-gossip-destination-public-key
   #:gossip-payload
   #:for-me-p
   #:handle-incoming-packet*
   #:peers
   #:gossip-seen-p
   #:gossip-id
   #:schedule-to-event-loop*
   #:send-gossip
   #:send-data
   #:receive-data
   #:handle-received-gossip
   #:fundamental-nest
   #:channel-peer
   #:channel-port-number
   #:channel-nest
   #:opened-message<-enveloped-gossip
   #:send-packet
   #:connect*
   #:disconnected
   #:connected
   #:stop-nest*
   #:fundamental-connection
   #:failed-to-connect
   #:nest-stopped
   #:nest-started
   #:with-main-lock-held
   #:start-nest*
   #:make-promise*
   #:enveloped-message<-opened-gossip
   #:fundamental-network-destination
   #:network-destination-peer
   #:schedule-to-event-loop/no-lock
   #:ping-at
   #:pong-at
   #:stop-networking
   #:start-networking
   #:networking
   #:packet
   #:+type-gossip+
   #:+type-ping+
   #:+type-pong+
   #:+type-route-discovery+
   #:+type-data+))

(cl:defpackage #:pantalea.transport.tcp
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:p #:pantalea.transport.protocol)
   (#:promise #:pantalea.utils.promise))
  (:export
   #:ip-destination
   #:networking))

(cl:defpackage #:pantalea.transport
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:p #:pantalea.transport.protocol)
   (#:tcp #:pantalea.transport.tcp)
   (#:q #:pantalea.utils.queue)
   (#:promise #:pantalea.utils.promise)
   (#:tw #:pantalea.utils.timing-wheel)
   (#:sk #:pantalea.utils.skip-list))
  (:export
   #:connection))
