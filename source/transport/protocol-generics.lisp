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


(defgeneric disconnected (nest connection reason))
(defgeneric connected (nest destination connection))
(defgeneric failed-to-connect (nest destination reason))
(defgeneric connect (nest destination))
(defgeneric disconnect (nest connection))
(defgeneric schedule-to-event-loop/no-lock (nest promise &optional delay))
(defgeneric schedule-to-event-loop (nest promise &optional delay))
(defgeneric stop-nest (nest))
(defgeneric start-nest (eest))
(defgeneric handle-incoming-packet (nest connection type packet))
(defgeneric send-packet (connection type packet))
(defgeneric spread-message (nest origin message source-public-key))
(defgeneric stop-networking (nest networking))
(defgeneric start-networking (nest networking))
(defgeneric networking-of-type (nest type))
(defgeneric destination-key (connection))
(defgeneric map-connections (networking function))
(defgeneric insert-direct-route (nest connection))
(defgeneric clear-dead-route (container dead-route))
(defgeneric message-active-p (nest message))
(defgeneric handle-incoming-message (nest connection message))
(defgeneric handle-incoming-response (nest handler response packet connection))
(defgeneric forget-message (nest id))
(defgeneric send-message (nest connection message))
(defgeneric send-response (nest connection message response))
(defgeneric discover-peers (nest))
(defgeneric networking-symbol (networking))
(defgeneric add-networking (nest networking))
