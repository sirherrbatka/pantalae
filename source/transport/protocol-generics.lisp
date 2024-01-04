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


(defgeneric gossip-seen-p (nest gossip))
(defgeneric peers (nest))
(defgeneric for-me-p (nest gossip))
(defgeneric send-gossip (controler nest gossip))
(defgeneric send-data (controler channel data))
(defgeneric receive-data (controler channel))
(defgeneric yield-channel (controler channel-group timeout))
(defgeneric handle-received-gossip (controler payload nest gossip))
(defgeneric gossip-payload (gossip))
(defgeneric close-channel (controler channel))
(defgeneric gossip-id (gossip))
(defgeneric enveloped-gossip-destination-public-key (enveloped-gossip))
(defgeneric opened-gossip<-enveloped-gossip (enveloped-gossip own-private-key))
(defgeneric enveloped-gossip<-opened-gosspi (opened-gossip destination-public-key))
(defgeneric peer-routes (peer))
(defgeneric discover-routes (peer))
(defgeneric force (promise)
  (:method ((future t))
    future))
(defgeneric completedp (promise))
(defgeneric fullfill (promise))
(defgeneric route-discovery-decrypt-payload (route-discovery private-key))
(defgeneric route-discovery-encrypt-payload (route-discovery public-key))

(defgeneric disconnected (nest connection reason))
(defgeneric connected (nest destination connection))
(defgeneric failed-to-connect (nest destination reason))
(defgeneric connect* (nest destination))
(defgeneric schedule-to-event-loop* (nest promise &optional delay))
(defgeneric stop-nest* (nest))
(defgeneric start-nest* (eest))
(defgeneric handle-incoming-packet* (nest connection type packet))
(defgeneric send-packet (connection type packet))
