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


(defun gossip-timestamp (gossip)
  (local-time:make-timestamp :day (day gossip)
                             :sec (sec gossip)
                             :nsce (nsec gossip)))

(defun make-double-ratchet-local-client (nest)
  (~> nest long-term-identity-key dr:make-local-client))

(defun send-keys (connection local-client)
  (~>> local-client
       dr:client-public-keys
       conspack:encode
       (send-packet connection +type-keys+)))

(defun set-double-ratchet (connection local-client remote-client-keys)
  (let ((remote-client (apply #'dr:make-remote-client remote-client-keys)))
    (setf (double-ratchet connection) (dr:make-double-ratchet local-client remote-client))))

(defun decrypt (connection packet)
  (bind (((key data) (conspack:decode packet)))
    (dr:decrypt (double-ratchet connection)
                data
                key
                0
                (length data)
                data)
    data))

(defun encrypt (connection packet &optional (result packet))
  (conspack:encode
   (pantalea.cryptography:encrypt
    (double-ratchet connection)
    packet
    0
    (length packet)
    result)))

(defun run-event-loop (nest)
  (handler-case
      (iterate
        (with queue = (event-loop-queue nest))
        (for callback = (q:blocking-queue-pop! queue))
        (if (typep callback 'promise:promise)
            (promise:fullfill! callback)
            (funcall callback)))
    (pantalea.utils.conditions:stop-thread (e)
      (declare (ignore e))
      (log4cl:log-info "Event loop has been stopped."))
    (error (e)
      (log4cl:log-error "Event loop has crashed with ~a" e))))

(defmacro make-response (message (payload-class &rest keys))
  (once-only (message)
    `(~>> (make ',payload-class ,@keys) conspack:encode
      (ironclad:encrypt-in-place (origin-public-key ,message))
      (make 'response :id (id ,message) :encrypted-payload _))))
