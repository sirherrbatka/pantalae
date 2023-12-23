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
   #:message-timestamp
   #:message-id
   #:enveloped-message-destination-public-key
   #:message-payload
   #:for-me-p
   #:peers
   #:message-seen-p
   #:message-id
   #:send-message
   #:handle-received-message
   #:opened-message<-enveloped-message
   #:enveloped-message<-opened-message))

(cl:defpackage #:pantalea.transport.skip-list
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:skip-list-drop!
   #:skip-list-insert!
   #:make-skip-list))

(cl:defpackage #:pantalea.transport
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:p #:pantalea.transport.protocol)
   (#:sk #:pantalea.transport.skip-list))
  (:export
   ))
