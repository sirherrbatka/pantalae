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


(alexandria:define-constant +type-gossip+ 0)
(alexandria:define-constant +type-ping+ 1)
(alexandria:define-constant +type-pong+ 2)
(alexandria:define-constant +type-route-discovery+ 3)
(alexandria:define-constant +type-data+ 4)
(alexandria:define-constant +type-keys+ 5)
(alexandria:define-constant +type-echo+ 6)
(alexandria:define-constant +type-message+ 7)
(alexandria:define-constant +type-response+ 8)
(alexandria:define-constant +timing-wheel-size+ 512)
(alexandria:define-constant +timing-wheel-tick-duration+ 50)
(alexandria:define-constant +empty-packet+ (make-array 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +ping-delay+ 60000) ; 1 minute
