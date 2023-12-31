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
  (p:schedule-to-event-loop* nest (promise:promise
                                 (log4cl:log-info "Nest Started.")))
  nest)

(defmethod print-object ((object ip-destination) stream)
  (print-unreadable-object (object stream)
    (format stream "HOST: ~a" (host object))))

(defmethod p:stop-nest* ((nest nest-implementation))
  (unless (started nest) (error 'p:nest-stopped))
  (~> nest
      networking
      socket-bundles
      (map 'list (lambda (bundle)
                   (bt:with-lock-held ((lock bundle))
                     (setf (terminating bundle) (promise:promise t))))
           _)
      promise:combine
      (list _
            (promise:promise (signal 'pantalea.utils.conditions:stop-thread)))
      promise:combine
      (p:schedule-to-event-loop* nest _)
      (list _
            (tw:add! (timing-wheel nest)
                     +timing-wheel-tick-duration+
                     (promise:promise (signal 'pantalea.utils.conditions:stop-thread))))
      promise:combine
      promise:force!)
  (setf (started nest) nil)
  (setf (~> nest networking socket-bundles fill-pointer) 0)
  (setf (event-loop-queue nest) (q:make-blocking-queue))
  (bt:join-thread (event-loop-thread nest))
  (tw:join-thread! (timing-wheel nest))
  (setf (event-loop-thread nest) nil)
  (setf (timing-wheel nest) nil)
  (log4cl:log-info "Nest has been stopped.")
  nest)

(defmethod p:schedule-to-event-loop* ((nest nest-implementation) promise &optional (delay 0))
  (if (zerop delay)
      (q:blocking-queue-push! (event-loop-queue nest)
                              promise)
      (tw:add! (timing-wheel nest) delay
               (lambda (tw) (declare (ignore tw))
                 (p:schedule-to-event-loop* nest promise))))
  promise)

(defmethod p:connect* ((nest nest-implementation) (destination ip-destination))
  (p:with-main-lock-held (nest)
    (unless (started nest) (error 'p:nest-stopped)))
  (let* ((connected (promise:promise nil))
         (failed (promise:promise nil))
         (result nil)
         (task (promise:promise
                 (let* ((socket-bundle (make 'socket-bundle :host (host destination)))
                        (on-success (promise:promise
                                      (vector-push-extend socket-bundle
                                                          (~> nest networking socket-bundles))
                                      (p:connected nest destination)
                                      (promise:fullfill! connected))))
                   (run-socket-bundle socket-bundle nest on-success failed destination)
                   (setf result socket-bundle)))))
    (p:schedule-to-event-loop* nest task)
    (if-let ((e (promise:find-fullfilled connected failed)))
      (error e)
      result)))

(defmethod p:disconnected ((nest nest-implementation) (destination ip-destination) reason)
  (log4cl:log-info "Connection to ~a lost because ~a." destination reason)
  (let* ((socket-bundles (~> nest networking socket-bundles))
         (last-index (~> socket-bundles length 1-))
         (index (position (host destination) socket-bundles :key #'host :test #'equal)))
    (when (null index)
      (log4cl:log-warn "Connection to ~a was not found in nest!" destination)
      (return-from p:disconnected nil))
    (rotatef (aref socket-bundles index) (aref socket-bundles last-index))
    (setf (aref socket-bundles last-index) nil)
    (decf (fill-pointer socket-bundles)))
  nil)

(defmethod p:connected ((nest nest-implementation) (destination ip-destination))
  (log4cl:log-info "Connection to ~a established." destination)
  nil)

(defmethod p:failed-to-connect ((nest nest-implementation) (destination ip-destination))
  (log4cl:log-error "Connection to ~a could not be established." destination)
  nil)
