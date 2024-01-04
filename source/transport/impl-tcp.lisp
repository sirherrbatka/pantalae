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


(defclass tcp-networking ()
  ((%socket-bundles
    :initarg :socket-bundles
    :accessor socket-bundles)
   (%server-socket
    :initarg :server-socket
    :accessor server-socket)
   (%server-thread
    :initarg :server-thread
    :accessor server-thread)
   (%terminating
    :initarg :terminating
    :accessor terminating)
   (%server-lock
    :initarg :server-lock
    :accessor server-lock)
   (%lock :initarg :lock
          :reader lock))
  (:default-initargs
   :lock (bt:make-lock "Sockets Lock.")
   :server-socket nil
   :server-lock (bt:make-lock "Server Lock.")
   :terminating nil
   :server-thread nil
   :socket-bundles (vect)))

(defclass socket-bundle (p:fundamental-connection)
  ((%socket
    :initarg :socket
    :accessor socket)
   (%thread
    :initarg :thread
    :accessor thread)
   (%terminating
    :initarg :terminating
    :accessor terminating)
   (%total-bytes
    :initarg :total-bytes
    :accessor total-bytes)
   (%start-time
    :initarg :start-time
    :accessor start-time)
   (%host
    :initarg :host
    :accessor host)
   (%lock
    :initarg :lock
    :accessor lock))
  (:default-initargs
   :lock (bt:make-lock)
   :host nil
   :socket nil
   :thread nil
   :total-bytes 0
   :start-time nil
   :terminating nil))

(defclass ip-destination (p:fundamental-network-destination)
  ((%host
    :initarg :host
    :reader host))
  (:default-initargs
   ))

(defmethod p:send-packet ((connection socket-bundle) type packet)
  (with-socket-bundle-locked (connection)
    (let ((stream (~> connection socket usocket:socket-stream)))
      (if (null stream)
          (progn
            (log4cl:log-warn "Attempting to send packet, but socket has expired.")
            nil)
          (let ((length (length packet)))
            (nibbles:write-ub16/be type stream)
            (nibbles:write-ub16/be length stream)
            (iterate
              (for i from 0 below length)
              (write-byte (aref packet i) stream)
              (finally (finish-output stream)
                       (return t))))))))

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

(defun run-socket-bundle (bundle nest on-succes on-fail destination)
  (setf (thread bundle)
        (bt:make-thread
         (curry #'run-socket-bundle-impl bundle nest on-succes on-fail destination)
         :name "Socket Thread")))

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
