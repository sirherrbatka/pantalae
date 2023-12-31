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
(cl:in-package #:pantalea.transport)


(defmethod p:start-nest* ((nest nest-implementation))
  (when (started nest) (error 'p:nest-started))
  (log4cl:log-info "Starting Nest.")
  (maphash-values (lambda (networking)
                    (p:start-networking nest networking))
                  (networking nest))
  (setf (event-loop-thread nest) (bt:make-thread (curry #'run-event-loop nest)
                                                 :name "Nest Event Loop Thread")
        (timing-wheel nest) (tw:run +timing-wheel-size+ +timing-wheel-tick-duration+)
        (started nest) t)
  (schedule-to-event-loop-impl nest (promise:promise
                                      (log4cl:log-info "Nest has been started.")))
  nest)

(defmethod p:stop-nest* ((nest nest-implementation))
  (unless (started nest) (error 'p:nest-stopped))
  (log4cl:log-info "Stopping nest.")
  (maphash-values (lambda (networking)
                    (p:stop-networking nest networking))
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

(defmethod p:schedule-to-event-loop/no-lock ((nest nest-implementation) promise &optional (delay 0))
  (schedule-to-event-loop-impl nest promise delay))

(defmethod p:connected ((nest nest-implementation) destination connection)
  (log4cl:log-info "Connection to ~a established." destination)
  (schedule-ping nest connection)
  nil)

(defmethod p:failed-to-connect ((nest nest-implementation) destination reason)
  (log4cl:log-error "Connection to ~a could not be established because ~a." destination reason)
  nil)

(defmethod p:handle-incoming-packet* ((nest nest-implementation) connection (type (eql p:+type-pong+)) packet)
  (log4cl:log-debug "Got pong.")
  (setf (p:pong-at connection) (local-time:now))
  (schedule-ping nest connection)
  nil)

(defmethod p:handle-incoming-packet* ((nest nest-implementation) connection (type (eql p:+type-ping+)) packet)
  (log4cl:log-debug "Got ping.")
  (p:schedule-to-event-loop* nest (curry #'send-pong connection))
  nil)

(defmethod p:networking ((nest nest-implementation) type)
  (gethash type (networking nest)))
