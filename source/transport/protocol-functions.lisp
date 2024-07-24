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
  (dr:decrypt (double-ratchet connection)
              (conspack:decode packet)))

(defun transcrypt (from-connection to-connection packet)
  (~>> packet
       (decrypt from-connection)
       (encrypt to-connection)))

(defun encrypt (connection packet)
  (~> connection
      double-ratchet
      (pantalea.cryptography:encrypt packet)
      conspack:encode))

(defun run-event-loop (nest)
  (iterate
    (with queue = (event-loop-queue nest))
    (for callback = (q:blocking-queue-pop! queue))
    (handler-case
        (if (typep callback 'promise:promise)
            (promise:fullfill! callback)
            (funcall callback))
      (pantalea.utils.conditions:stop-thread (e)
        (declare (ignore e))
        (log4cl:log-info "Event loop has been stopped.")
        (return-from run-event-loop nil))
      (error (e)
        (log4cl:log-error "Error on the nest event loop thread ~a" e)))))

(defun decrypt-payload (nest response)
  (bind (((:accessors encrypted-payload) response)
         (origin-public-key (envelop-origin response (long-term-identity-key nest)))
         (payload (~> nest long-term-identity-key dr:private
                      (ironclad:diffie-hellman origin-public-key)
                      (ironclad:make-cipher :blowfish :mode :ecb :key _)
                      (decrypt-in-place encrypted-payload)
                      conspack:decode)))
    payload))

(defun send-echo (connection)
  (lret ((bytes (ironclad:make-random-salt 20)))
    (pantalea.transport.protocol:send-packet connection
                                             pantalea.transport.protocol:+type-echo+
                                             (encrypt connection bytes))))

(defun connections-count (nest)
  (let ((result 0))
    (map-connections nest (lambda (connection)
                            (declare (ignore connection))
                            (incf result)))
    result))

(defun validate-connection-encryption (connection read-connection)
  (flet ((send-own-random ()
           (bind ((random-data (send-echo connection))
                  ((:values type buffer) (funcall read-connection))
                  (decrypted (decrypt connection buffer)))
             (assert (= type +type-echo+))
             (unless (vector= random-data decrypted)
               (error 'cryptographic-handshake-failed))))
         (handle-random ()
           (bind (((:values type buffer) (funcall read-connection))
                  (decrypted (decrypt connection buffer)))
             (assert (= type +type-echo+))
             (pantalea.transport.protocol:send-packet connection
                                                      pantalea.transport.protocol:+type-echo+
                                                      (encrypt connection decrypted)))))
    (if (pantalea.cryptography:can-encrypt-p (double-ratchet connection))
        (progn (send-own-random) (handle-random))
        (progn (handle-random) (send-own-random)))
    (log4cl:log-info "Connection encryption established.")))
