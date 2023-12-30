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

(defun run-socket-bundle-impl (bundle nest on-success on-fail)
  (handler-case
      (progn
        (setf (start-time bundle) (local-time:now)
              (socket bundle) (usocket:socket-connect (host bundle)
                                                      +tcp-port+
                                                      :element-type '(unsigned-byte 8)
                                                      :timeout +tcp-timeout+))
        (promise:fullfill! on-success))
    (error (e)
      (promise:fullfill! on-fail :value e :success nil)
      (error e)))
  (unwind-protect
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
           (p:event-loop-schedule* nest
                                   (curry #'p:handle-incoming-packet
                                          nest
                                          buffer))
           (bt:with-lock-held ((lock bundle))
             (incf (total-bytes bundle) length))
           (setf start 0 length nil buffer nil)))
    (if-let ((socket (socket bundle)))
      (usocket:socket-close socket))
    (bt:with-lock-held ((lock bundle))
      (if-let ((terminating (terminating bundle)))
        (promise:fullfill! terminating)))))

(defun run-socket-bundle (bundle nest on-succes on-fail)
  (setf (thread bundle)
        (bt:make-thread
         (curry #'run-socket-bundle-impl bundle nest on-succes on-fail)
         :name "Socket Thread")))

(defun run-event-loop (nest)
  (handler-case
      (iterate
        (with queue = (event-loop-queue nest))
        (for callback = (q:blocking-queue-pop! queue))
        (if (typep callback 'promise:promise)
            (promise:fullfill! callback)
            (funcall callback)))
    (pantalea.utils.conditions:stop-thread nil)))
