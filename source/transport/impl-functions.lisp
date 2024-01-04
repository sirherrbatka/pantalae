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


;; method locks main-nest-lock, this function is called from stop/start -nest methods to avoid deadlock Tue Jan  2 15:29:45 2024
(defun schedule-to-event-loop-impl (nest promise &optional (delay 0))
  (unless (started nest) (error 'p:nest-stopped))
  (if (zerop delay)
      (q:blocking-queue-push! (event-loop-queue nest)
                              promise)
      (tw:add! (timing-wheel nest) delay
               (lambda (&rest rest) (declare (ignore rest))
                 (p:schedule-to-event-loop* nest promise))))
  promise)

(defun gossip-signature (gossip)
  (cons (p:gossip-timestamp gossip)
        (p:gossip-id gossip)))

(defun signature< (a b)
  (bind (((timestamp-a . id-a) a)
         ((timestamp-b . id-b) b))
    (if (local-time:timestamp>= timestamp-a timestamp-b)
        nil
        (< id-a id-b))))

(defun signature= (a b)
  (bind (((timestamp-a . id-a) a)
         ((timestamp-b . id-b) b))
    (if (local-time:timestamp= timestamp-a timestamp-b)
        nil
        (= id-a id-b))))

(defun insert-socket-bundle (nest socket-bundle destination)
  (handler-case
      (progn
        (ensure (socket socket-bundle) (usocket:socket-connect (host socket-bundle)
                                                               +tcp-port+
                                                               :element-type '(unsigned-byte 8)
                                                               :timeout +tcp-timeout+))
        (setf (host socket-bundle) (~> socket-bundle socket usocket:get-peer-address))
        (let* ((connected (promise:promise nil))
               (result nil)
               (host (host socket-bundle))
               (on-success (promise:promise
                             (schedule-to-event-loop-impl nest (curry #'p:connected nest destination result))
                             (promise:fullfill! connected)))
               (failed (promise:promise
                         (setf (~> nest networking socket-bundles last-elt) nil)
                         (decf (~> nest networking socket-bundles fill-pointer))
                         nil)))
          (bt:with-lock-held ((~> nest networking lock))
            (vector-push-extend socket-bundle
                                (~> nest networking socket-bundles))
            (let* ((position (position host (~> nest networking socket-bundles) :test 'equalp :key #'host)))
              (if (= position (~> nest networking socket-bundles length 1-))
                  (let ((socket-bundle (~> nest networking socket-bundles last-elt)))
                    (log4cl:log-info "Adding new connection for ~a." host)
                    (setf result socket-bundle)
                    (run-socket-bundle socket-bundle
                                       nest
                                       (promise:promise
                                         (schedule-to-event-loop-impl nest on-success))
                                       (promise:promise
                                         (schedule-to-event-loop-impl nest failed))
                                       destination))
                  (progn
                    (log4cl:log-info "Using existing connection for ~a." host)
                    (usocket:socket-close (socket socket-bundle))
                    (setf (~> nest networking socket-bundles last-elt) nil)
                    (decf (~> nest networking socket-bundles fill-pointer))
                    (setf result (aref (~> nest networking socket-bundles) position))
                    (return-from insert-socket-bundle result)))))
          (bind (((:values fullfilled number) (promise:find-fullfilled connected failed)))
            (declare (ignore fullfilled))
            (when (= number 1)
              (error "Could not connect!")))
          result))
    (error (e)
      (when-let ((socket (socket socket-bundle)))
        (usocket:socket-close socket)
        (setf (socket socket-bundle) nil))
      (signal e))))

(defun run-server-socket-impl (nest &aux (networking (networking nest)) e)
  (log4cl:log-info "Starting server thread.")
  (unwind-protect
       (handler-case
           (iterate
             (with lock = (server-lock networking))
             (with socket = (server-socket networking))
             (for terminating = (bt:with-lock-held (lock) (terminating networking)))
             (when terminating (leave))
             (for r-socket = (usocket:wait-for-input socket :timeout 1 :ready-only t))
             (when (null r-socket) (next-iteration))
             (log4cl:log-info "Accepting incoming connection.")
             (for active-socket = (usocket:socket-accept socket))
             (for host = (usocket:get-peer-address active-socket))
             (log4cl:log-info "Incoming connection for ~a." host)
             (for socket-bundle = (make 'socket-bundle :host host :socket active-socket))
             (handler-case
                 (insert-socket-bundle nest socket-bundle (make 'ip-destination :host host))
               (error (e) (log4cl:log-error "~a" e)))))
    (bt:with-lock-held ((server-lock networking))
      (when-let ((terminating (terminating networking)))
        (setf e :terminated)
        (usocket:socket-close (server-socket networking))
        (promise:fullfill! terminating)))
    (log4cl:log-info "Server thread has been stopped because ~a" e)))

(defun run-server-socket (nest)
  (bind (((:accessors server-socket server-thread) (networking nest)))
    (handler-case
        (setf server-socket (usocket:socket-listen usocket:*wildcard-host*
                                                   +tcp-port+
                                                   :reuse-address t
                                                   :element-type '(unsigned-byte 8))
              server-thread (bt:make-thread (curry #'run-server-socket-impl nest)
                                            :name "Nest server socket thread."))
      (error (e)
        (log4cl:log-error "Can't start server-socket ~a." e)
        (error e)))))

(defun run-socket-bundle-impl (bundle nest on-success on-fail destination &aux (e nil))
  (handler-case
      (progn
        (setf (start-time bundle) (local-time:now))
        (ensure (socket bundle) (usocket:socket-connect (host bundle)
                                                        +tcp-port+
                                                        :element-type '(unsigned-byte 8)
                                                        :timeout +tcp-timeout+))
        (promise:fullfill! on-success))
    (error (e)
      (promise:fullfill! on-fail :value e :success nil)
      (schedule-to-event-loop-impl nest (promise:promise (p:failed-to-connect nest destination e)))
      (error e)))
  (unwind-protect
       (handler-case
           (iterate
             (with socket = (socket bundle))
             (with lock = (lock bundle))
             (for terminating = (bt:with-lock-held (lock)
                                  (terminating bundle)))
             (when terminating (leave))
             (for r-socket = (usocket:wait-for-input socket :timeout 1 :ready-only t))
             (when (null r-socket) (next-iteration))
             (for buffer = nil)
             (for type = nil)
             (bt:with-lock-held (lock)
               (let ((stream (usocket:socket-stream socket)))
                 (setf type (nibbles:read-ub16/be stream))
                 (bind ((length (nibbles:read-ub16/be stream)))
                   (setf buffer (make-array length :element-type '(unsigned-byte 8)))
                   (iterate
                     (for i from 0 below length)
                     (setf (aref buffer i) (read-byte stream)))
                   (incf (total-bytes bundle) length))))
             (schedule-to-event-loop-impl nest
                                          (curry #'p:handle-incoming-packet*
                                                 nest
                                                 bundle
                                                 type
                                                 buffer)))
         (error (er)
           (setf e er)))
    (bt:with-lock-held ((lock bundle))
      (when-let ((socket (socket bundle)))
        (ignore-errors (usocket:socket-close socket))
        (setf socket nil)))
    (bt:with-lock-held ((lock bundle))
      (when-let ((terminating (terminating bundle)))
        (setf e :terminated)
        (promise:fullfill! terminating)))
    (unless (member e '(nil :terminated))
      (log4cl:log-error "Socket thread has been stopped because ~a" e))
    (schedule-to-event-loop-impl nest
                                 (promise:promise
                                   (p:disconnected nest bundle e)))))

(defun socket-bundle-send-packet (connection type packet)
  (with-socket-bundle-locked (connection)
    (let ((stream (~> connection socket usocket:socket-stream)))
      (if (null stream)
          nil
          (let ((length (length packet)))
            (nibbles:write-ub16/be type stream)
            (nibbles:write-ub16/be length stream)
            (iterate
              (for i from 0 below length)
              (write-byte (aref packet i) stream)
              (finally (finish-output stream)
                       (return t))))))))

(defun send-ping (connection)
  (socket-bundle-send-packet connection p:+type-ping+ +empty-packet+))

(defun send-pong (connection)
  (socket-bundle-send-packet connection p:+type-pong+ +pong-packet+))

(defun schedule-ping (nest connection)
  (flet ((pinging ()
           (unless (send-ping connection)
             (log4cl:log-warn "Ping not send because socket is disconnected."))))
    (log4cl:log-debug "Scheduling ping!")
    (p:schedule-to-event-loop* nest
                               #'pinging
                               +ping-delay+)))

(defun run-socket-bundle (bundle nest on-succes on-fail destination)
  (setf (thread bundle)
        (bt:make-thread
         (curry #'run-socket-bundle-impl bundle nest on-succes on-fail destination)
         :name "Socket Thread")))

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
