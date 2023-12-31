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


(defmethod p:nest-start* ((nest nest-implementation))
  (unless (started nest)
    (setf (event-loop-thread nest) (bt:make-thread (curry #'run-event-loop nest)
                                                   :name "Nest Event Loop Thread")
          (timing-wheel nest) (tw:run +timing-wheel-size+ +timing-wheel-tick-duration+)
          (started nest) t))
  nest)

(defmethod p:nest-stop* ((nest nest-implementation))
  (unless (started nest)
    (return-from p:nest-stop* nest))
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
      (p:event-loop-schedule* nest _)
      (list _
            (tw:add! (timing-wheel nest)
                     +timing-wheel-tick-duration+
                     (promise:promise (signal 'pantalea.utils.conditions:stop-thread))))
      promise:combine
      promise:force!)
  (setf (started nest) nil)
  (setf (~> nest networking socket-bundles fill-pointer) 0)
  (setf (~> nest event-loop-queue) (q:make-blocking-queue))
  (bt:join-thread (~> nest event-loop-thread))
  (tw:join-thread! (timing-wheel nest))
  (setf (~> nest event-loop-thread) nil)
  nest)

(defmethod p:event-loop-schedule* ((nest nest-implementation) promise &optional (delay 0))
  (if (zerop delay)
      (q:blocking-queue-push! (event-loop-queue nest)
                              promise)
      (tw:add! (timing-wheel nest) delay
               (lambda (tw) (declare (ignore tw))
                 (p:event-loop-schedule* nest promise))))
  promise)

(defmethod p:connect ((nest nest-implementation) (destination ip-destination))
  (let* ((socket-bundle (make 'socket-bundle :host (host destination)))
        (connected (promise:promise nil))
        (on-success (promise:promise
                     (p:event-loop-schedule* nest
                                             (promise:promise
                                               (vector-push-extend socket-bundle
                                                                   (~> nest networking socket-bundles))
                                               (promise:fullfill! connected)))))
        (failed (promise:promise nil)))
    (run-socket-bundle socket-bundle nest on-success failed)
    (promise:eager-promise
      (if-let ((e (promise:find-fullfilled connected failed)))
        (error e)
        nil))))

(defmethod p:disconnected ((nest nest-implementation) (destination ip-destination) reason)
  (print reason)
  nil)

(defmethod p:connected ((nest nest-implementation) (destination ip-destination))
  nil)
