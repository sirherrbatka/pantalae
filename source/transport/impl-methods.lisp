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
  (setf (event-loop-thread nest) (bt:make-thread (curry #'run-event-loop nest)
                                                 :name "Nest Event Loop Thread")
        (timing-wheel nest) (tw:run +timing-wheel-size+ +timing-wheel-tick-duration+)
        (started nest) t)
  (run-server-socket nest)
  (schedule-to-event-loop-impl nest (promise:promise
                                      (log4cl:log-info "Nest has been started.")))
  nest)

(defmethod print-object ((object ip-destination) stream)
  (print-unreadable-object (object stream)
    (format stream "HOST: ~a" (host object))))

(defmethod p:stop-nest* ((nest nest-implementation))
  (unless (started nest) (error 'p:nest-stopped))
  (log4cl:log-info "Stopping nest.")
  ;; first, let's stop all socket threads Tue Jan  2 15:18:33 2024
  (log4cl:log-info "Stopping sockets.")
  (~> nest
      networking
      socket-bundles
      (map 'list (lambda (bundle)
                   (prog1 (bt:with-lock-held ((lock bundle))
                            (setf (terminating bundle) (promise:promise t)))
                     (~> bundle thread bt:join-thread)))
           _)
      promise:combine
      (schedule-to-event-loop-impl nest _)
      promise:force!)
  (setf (~> nest networking socket-bundles fill-pointer) 0)
  ;; stop the server thread Tue Jan  2 15:18:50 2024
  (log4cl:log-info "Stopping server.")
  (promise:force! (bt:with-lock-held ((~> nest networking server-lock))
                    (setf (~> nest networking terminating) (promise:promise t))))
  (~> nest networking server-thread bt:join-thread)
  (setf (~> nest networking server-thread) nil
        (~> nest networking server-socket) nil)
  ;; finally stop event loop and timing wheel Tue Jan  2 15:19:11 2024
  (ignore-errors (promise:force! (schedule-to-event-loop-impl nest (promise:promise
                                                                     (signal 'pantalea.utils.conditions:stop-thread)))))
  (ignore-errors (promise:force! (tw:add! (timing-wheel nest)
                                          +timing-wheel-tick-duration+
                                          (promise:promise (signal 'pantalea.utils.conditions:stop-thread)))))
  (bt:join-thread (event-loop-thread nest))
  (tw:join-thread! (timing-wheel nest))
  ;; everything should be stopped now, resetting state Tue Jan  2 15:24:41 2024
  (setf (started nest) nil)
  (setf (event-loop-queue nest) (q:make-blocking-queue))
  (setf (event-loop-thread nest) nil)
  (setf (timing-wheel nest) nil)
  (log4cl:log-info "Nest has been stopped.")
  nest)

(defmethod p:schedule-to-event-loop* ((nest nest-implementation) promise &optional (delay 0))
  (schedule-to-event-loop-impl nest promise delay))

(defmethod p:connect* ((nest nest-implementation) (destination ip-destination))
  (insert-socket-bundle nest (make 'socket-bundle :host (host destination)) destination))

(defmethod p:disconnected ((nest nest-implementation) (connection socket-bundle) reason)
  (log4cl:log-info "Connection to ~a lost because ~a." (host connection) reason)
  (bt:with-lock-held ((~> nest networking lock))
    (let* ((socket-bundles (~> nest networking socket-bundles))
           (last-index (~> socket-bundles length 1-))
           (index (position connection socket-bundles :test #'eq)))
      (when (null index)
        (log4cl:log-warn "Connection to ~a was not found in nest!" (host connection))
        (return-from p:disconnected nil))
      (rotatef (aref socket-bundles index) (aref socket-bundles last-index))
      (setf (aref socket-bundles last-index) nil)
      (decf (fill-pointer socket-bundles))))
  nil)

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
