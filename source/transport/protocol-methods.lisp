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
  (bt:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod stop-nest :around ((nest nest))
  (bt:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod connect :around ((nest nest) destination)
  (bt:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod schedule-to-event-loop ((nest nest) promise &optional (delay 0))
  (bt:with-lock-held ((main-nest-lock nest))
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
         (new-route (make 'direct-route :connection connection
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
  (setf (event-loop-thread nest) (bt:make-thread (curry #'run-event-loop nest)
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
  (ignore-errors (bt:join-thread (event-loop-thread nest)))
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
  (log4cl:log-info "Echo: ~a." (~>> packet (decrypt connection) conspack:decode)))

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
  (bind ((message (~>> packet (decrypt connection) conspack:decode)))
    (handle-incoming-message nest connection message)))

(defmethod spread-message ((nest nest) origin message source-public-key)
  (map-connections nest
                   (lambda (connection &aux (key (destination-public-key connection)))
                     (unless (equalp (ironclad:curve25519-key-y key)
                                     (ironclad:curve25519-key-y source-public-key))
                       (send-message nest connection message)))))

(defmethod message-active-p ((nest nest) message)
  (not (null (gethash (id message)
                      (~> nest message-table active-messages)))))

(defmethod handle-incoming-message :around ((nest nest) connection message)
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
                                     connection
                                     (handler message-handler)
                                     response)
  )

(defmethod handle-incoming-message ((nest nest)
                                    connection
                                    (message peer-discovery-request))
  (send-response nest connection message
                 (make 'peer-discovery-response
                       :origin-public-key (long-term-identity-key nest)
                       :id (id message)
                       :destination (destination message))))

(defmethod send-response ((nest nest) connection message response)
  (send-packet connection
               +type-response+
               (ironclad:encrypt-in-place (origin-public-key message)
                                          (conspack:encode response))))

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
  )
