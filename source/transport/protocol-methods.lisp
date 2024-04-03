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


(defmethod start-nest :around ((nest nest))
  (bt2:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod stop-nest :around ((nest nest))
  (bt2:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod connect :around ((nest nest) destination)
  (bt2:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod schedule-to-event-loop ((nest nest) promise &optional (delay 0))
  (bt2:with-lock-held ((main-nest-lock nest))
    (schedule-to-event-loop/no-lock nest promise delay)))

(defmethod disconnected :after ((nest nest) (connection fundamental-connection) reason)
  (pantalea.utils.dependency:kill connection))

(defmethod send-packet ((connection dead-connection) type packet)
  nil)

(defmethod send-packet :around ((connection fundamental-connection) type packet)
  (pantalea.utils.dependency:with-lock-held (connection)
    (call-next-method)))

(defmethod pantalea.utils.dependency:dead-class ((connection fundamental-connection))
  'dead-connection)

(defmethod disconnect ((nest nest) (connection dead-connection))
  nil)

(defmethod destination-key ((connection fundamental-connection))
  (~> connection double-ratchet dr:long-term-identity-remote-key))

(defmethod insert-direct-route ((nest nest) connection)
  (bind ((key (destination-key connection))
         (routing-table (routing-table nest))
         (own-routes (own-routes routing-table))
         (container (ensure (gethash key own-routes)
                      (make 'route-container
                            :routing-table routing-table
                            :content nil)))
         (new-route (make 'direct-route
                          :connection connection
                          :destination-public-key key)))
    (pantalea.utils.dependency:depend new-route connection)
    (pantalea.utils.dependency:depend container new-route)
    (when-let ((old-route (content container)))
      (pantalea.utils.dependency:undepend container old-route)
      (pantalea.utils.dependency:kill old-route))
    (setf (content container) new-route)
    nil))

(defmethod connection ((route route-container))
  (~> route content connection))

(defmethod destination-public-key ((route route-container))
  (~> route content destination-public-key))

(defmethod established-promise ((route route-container))
  (~> route content established-promise))

(defmethod timeout-promise ((route route-container))
  (~> route content timeout-promise))

(defmethod pantalea.utils.dependency:dead-class ((object own-route))
  'dead-own-route)

(defmethod pantalea.utils.dependency:kill ((object route-container))
  (clear-dead-route object (content object))
  (call-next-method))

(defmethod clear-dead-route ((container route-container) (dead-route own-route))
  (log:info "Clearing dead route.")
  (remhash (destination-public-key dead-route)
           (~> container routing-table own-routes))
  nil)

(defmethod start-nest ((nest nest))
  (when (started nest) (error 'nest-started))
  (log4cl:log-info "Starting Nest.")
  (maphash-values (lambda (networking)
                    (start-networking nest networking))
                  (networking nest))
  (setf (event-loop-thread nest) (bt2:make-thread (curry #'run-event-loop nest)
                                                 :name "Nest Event Loop Thread")
        (timing-wheel nest) (tw:run +timing-wheel-size+ +timing-wheel-tick-duration+)
        (started nest) t)
  (schedule-to-event-loop-impl nest (promise:promise
                                      (log4cl:log-info "Nest has been started.")))
  nest)

(defmethod stop-nest ((nest nest))
  (unless (started nest) (error 'nest-stopped))
  (log4cl:log-info "Stopping nest.")
  (maphash-values (lambda (networking)
                    (stop-networking nest networking))
                  (networking nest))
  ;; finally stop event loop and timing wheel Tue Jan  2 15:19:11 2024
  (ignore-errors (promise:force! (schedule-to-event-loop-impl nest (promise:promise
                                                                     (signal 'pantalea.utils.conditions:stop-thread)))))
  (ignore-errors (promise:force! (tw:add! (timing-wheel nest)
                                          +timing-wheel-tick-duration+
                                          (promise:promise (signal 'pantalea.utils.conditions:stop-thread)))))
  (ignore-errors (bt2:join-thread (event-loop-thread nest)))
  (tw:join-thread! (timing-wheel nest))
  ;; everything should be stopped now, resetting state Tue Jan  2 15:24:41 2024
  (setf (started nest) nil)
  (setf (event-loop-queue nest) (q:make-blocking-queue))
  (setf (event-loop-thread nest) nil)
  (setf (timing-wheel nest) nil)
  (log4cl:log-info "Nest has been stopped.")
  nest)

(defmethod schedule-to-event-loop/no-lock ((nest nest) promise &optional (delay 0))
  (schedule-to-event-loop-impl nest promise delay))

(defmethod connected ((nest nest) destination connection)
  (log4cl:log-info "Connection to ~a established." destination)
  (schedule-ping nest connection)
  (insert-direct-route nest connection)
  nil)

(defmethod failed-to-connect ((nest nest) destination reason)
  (log4cl:log-error "Connection to ~a could not be established because ~a." destination reason)
  nil)

(defmethod handle-incoming-packet ((nest nest) connection (type (eql +type-pong+)) packet)
  (log4cl:log-debug "Got pong.")
  (setf (pong-at connection) (local-time:now))
  (when-let ((pong-timeout-promise (pong-timeout-promise connection)))
    (promise:cancel! pong-timeout-promise))
  (schedule-ping nest connection)
  nil)

(defmethod handle-incoming-packet ((nest nest) connection (type (eql +type-echo+)) packet)
  (log4cl:log-info "Echo decrypted: ~a." (decrypt connection packet)))

(defmethod handle-incoming-packet ((nest nest) connection (type (eql +type-ping+)) packet)
  (log4cl:log-debug "Got ping.")
  (schedule-to-event-loop nest (curry #'send-pong connection))
  nil)

(defmethod networking-of-type ((nest nest) type)
  (gethash type (networking nest)))

(defmethod handle-incoming-packet ((nest nest)
                                   connection
                                   (type (eql +type-message+))
                                   packet)
  (log4cl:log-debug "Got message packet!")
  (bind ((decrypted-message (decrypt connection packet)))
    (~>> decrypted-message
         conspack:decode
         (handle-incoming-message nest connection))))

(defmethod handle-incoming-packet ((nest nest)
                                   connection
                                   (type (eql +type-response+))
                                   packet)
  (log4cl:log-debug "Got response packet!")
  (bind ((response (~>> packet conspack:decode))
         (handler (gethash (id response) (~>> nest message-table active-messages))))
    (unless (null handler)
      (handle-incoming-response nest handler response packet connection))))

(defmethod spread-message ((nest nest) origin message source-public-key)
  (map-connections nest
                   (lambda (connection &aux (key (destination-public-key connection)))
                     (unless (equalp (ironclad:curve25519-key-y key)
                                     (ironclad:curve25519-key-y source-public-key))
                       (send-message nest connection message)))))

(defmethod message-active-p ((nest nest) message)
  (not (null (gethash (id message)
                      (~> nest message-table active-messages)))))

(defmethod handle-incoming-message :around ((nest nest) connection (message message))
  (unless (message-active-p nest message)
    (let ((message-handler (make 'message-handler
                                 :id (id message)
                                 :nest nest
                                 :connection connection)))
      (pantalea.utils.dependency:depend message-handler connection)
      (setf (gethash (id message) (~> nest message-table active-messages)) message-handler))
    (unwind-protect
         (progn (call-next-method)
                (schedule-to-event-loop nest
                                        (let ((id (id message)))
                                          (lambda () (forget-message nest id)))
                                        #.(* 10 60 1000))) ; 10 minutes Fri Jan 19 22:05:46 2024
      (bind (((:accessors hop-counter) message))
        (when (<= (incf hop-counter) 8)
          (spread-message nest
                          (destination-public-key connection)
                          message
                          (destination-key connection)))))))

(defmethod handle-incoming-response ((nest nest)
                                     (handler message-handler)
                                     response
                                     packet
                                     connection)
  ;; should forward message to the origin Wed Jan 31 14:52:29 2024
  (send-packet (connection handler) +type-response+ packet))

(defmethod handle-incoming-response ((nest nest)
                                     (handler peer-discovery-handler)
                                     response
                                     packet
                                     connection)
  ;; connect, maybe? Wed Jan 31 15:01:00 2024
  (log4cl:log-debug "Got peer discovery response!")
  (let ((payload (decrypt-payload nest response)))
    (push (list (connected-peers payload) (destination payload) (origin-public-key response))
          (responses handler))))

(defmethod handle-incoming-message ((nest nest)
                                    connection
                                    (message peer-discovery-request))
  (log4cl:log-debug "Got peer discovery request!")
  (when (< (connections-count nest) (maximum-connections-count nest))
    (~> (make-response (long-term-identity-key nest)
                       message 'peer-discovery-payload
                       :connected-peers (connected-peers-sketch nest)
                       :destination (destination message))
        (send-response nest connection message _))))

(defmethod send-response ((nest nest) connection message response)
  (~>> response conspack:encode
       (send-packet connection +type-response+)))

(defmethod forget-message ((nest nest) id)
  (remhash id (~> nest message-table active-messages))
  nil)

(defmethod map-connections ((networking nest) function)
  (maphash-values (lambda (networking) (map-connections networking function))
                  (networking networking))
  nil)

(defmethod pantalea.utils.dependency:kill ((cell message-handler))
  (forget-message (read-nest cell) (id cell))
  (call-next-method))

(defmethod send-message ((nest nest) connection message)
  (setf (destination message) (destination connection))
  (~>> (conspack:encode message)
       (encrypt connection)
       (send-packet connection +type-message+)))

(defmethod discover-peers ((nest nest))
  (let* ((public-key (~> nest long-term-identity-key pantalea.cryptography:public))
         (message (make 'peer-discovery-request :origin-public-key public-key))
         (id (id message))
         (message-handler (make 'peer-discovery-handler :id id :nest nest)))
    (setf (gethash (id message) (~> nest message-table active-messages)) message-handler)
    (schedule-to-event-loop nest
                            (lambda  ()      ; maximum distance first
                              (let ((connected-peers-sketch (connected-peers-sketch nest))
                                    (connections-count (connections-count nest))
                                    (maximum-connections-count (maximum-connections-count nest))
                                    (new-connections-count (max 0 (- maximum-connections-count connections-count)))
                                    (responses (~>> (responses message-handler)
                                                    (mapcar (lambda (data)
                                                              (let ((distance (bloom:jaccard connected-peers-sketch
                                                                                             (first data))))
                                                                (log:debug "Distance to ~a: ~a" (second data) distance)
                                                                (cons distance (rest data)))))
                                                    (sort _ #'> :key #'first))))
                                (log:info "Connecting to discovered peers.")
                                (iterate
                                  (declare (ignorable key score))
                                  (while (<= connected new-connections-count))
                                  (for (score destination key) in responses)
                                  (counting (nth-value 1 (connect nest destination)) into connected)))
                              (forget-message nest id))
                            #.(* 5 60 1000))
    (spread-message nest public-key message public-key)))

(defmethod destination-public-key ((connection fundamental-connection))
  (~> connection
      double-ratchet
      pantalea.cryptography:remote-client
      pantalea.cryptography:long-term-identity-key
      pantalea.cryptography:public))

(defmethod add-networking ((nest nest) networking)
  (bt2:with-lock-held ((main-nest-lock nest))
    (when (gethash (networking-symbol networking) (networking nest))
      (error 'networking-already-present))
    (setf (gethash (networking-symbol networking) (networking nest)) networking))
  nest)
