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


(defun gossip-signature (gossip)
  (cons (p:gossip-timestamp message)
        (p:gossip-id message)))

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
  (ensure (socket socket-bundle) (usocket:socket-connect (host socket-bundle)
                                                         +tcp-port+
                                                         :element-type '(unsigned-byte 8)
                                                         :timeout +tcp-timeout+))
  (setf (host socket-bundle) (~> socket-bundle socket usocket:get-peer-address))
  (let* ((connected (promise:promise nil))
         (result nil)
         (host (host socket-bundle))
         (on-success (promise:promise
                       (p:connected nest destination result)
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
                                 failed
                                 destination))
            (progn
              (log4cl:log-info "Using existing connection for ~a." host)
              (usocket:socket-close (socket socket-bundle))
              (setf (~> nest networking socket-bundles last-elt) nil)
              (decf (~> nest networking socket-bundles fill-pointer))
              (setf result (aref (~> nest networking socket-bundles) position))
              (return-from insert-socket-bundle result))))
      (if-let ((e (promise:find-fullfilled connected failed)))
        (error e)
        result))))

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
             (log4cl:log-info "Accepting incoming connection." host)
             (for active-socket = (usocket:socket-accept r-socket :element-type '(unsigned-byte 8)))
             (for host = (usocket:get-peer-address socket))
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
             (with length = nil)
             (with socket = (socket bundle))
             (with lock = (lock bundle))
             (with buffer = nil)
             (with start = 0)
             (for terminating = (bt:with-lock-held (lock)
                                  (terminating bundle)))
             (when terminating (leave))
             (for r-socket = (usocket:wait-for-input socket :timeout 1 :ready-only t))
             (when (null r-socket) (next-iteration))
             (if (null length)
                 (setf length (~> socket
                                  usocket:socket-stream
                                  nibbles:read-ub16/be))
                 (progn
                   (when (null buffer)
                     (setf buffer (make-array length :element-type '(unsigned-byte 8))))
                   (bind ((new-size (- length start))
                          (buffer-view (make-array new-size
                                                   :displaced-to buffer
                                                   :element-type '(unsigned-byte 8)
                                                   :displaced-index-offset start))
                          ((:values buffer size host port)
                           (bt:with-lock-held (lock)
                             (usocket:socket-receive socket
                                                     buffer-view
                                                     new-size))))
                     (declare (ignorable buffer host port))
                     (incf start size))))
             (when (= start length)
               (schedule-to-event-loop-impl nest
                                            (curry #'p:handle-incoming-packet
                                                   nest
                                                   buffer))
               (bt:with-lock-held ((lock bundle))
                 (incf (total-bytes bundle) length))
               (setf start 0 length nil buffer nil)))
         (error (er)
           (setf e er)))
    (if-let ((socket (socket bundle)))
      (ignore-errors (usocket:socket-close socket)))
    (bt:with-lock-held ((lock bundle))
      (when-let ((terminating (terminating bundle)))
        (setf e :terminated)
        (promise:fullfill! terminating)))
    (log4cl:log-info "Socket thread has been stopped because ~a" e)
    (schedule-to-event-loop-impl nest
                                 (promise:promise
                                   (p:disconnected nest destination e)))))

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
      (log4cl:log-info "Event loop has been stopped."))))
