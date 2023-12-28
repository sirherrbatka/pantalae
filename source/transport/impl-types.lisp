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


(defclass socket-bundle ()
  ((%socket
    :initarg :socket
    :accessor socket)
   (%thread
    :initarg :thread
    :accessor thread)
   (%terminating
    :initarg :terminating
    :accessor terminating)
   (%lock
    :initarg :lock
    :accessor lock))
  (:default-initargs
   :terminating nil))

(defun run-socket-bundle (bundle nest)
  (setf (thread bundle)
        (bt:make-thread
         (lambda ()
           (iterate
             (with length = nil)
             (with socket = (socket bundle))
             (with lock = (lock bundle))
             (with buffer = nil)
             (with start = 0)
             (for terminating = (terminating bundle))
             (when terminating
               (promise:fullfill! terminating)
               (usocket:socket-close socket)
               (leave))
             (for r-socket = (usocket:wait-for-input socket :timeout 0.5))
             (when (null r-socket) (next-iteration))
             (if (null length)
                 (bt:with-lock-held (lock)
                   (setf length (~> socket
                                    usocket:socket-stream
                                    nibbles:read-ub32/be)))
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
                                       (promise:promise
                                         (p:handle-incoming-packet nest
                                                                   buffer)))
               (setf start 0 length nil buffer nil)))))))

(defclass networking ()
  ((%socket-bundles
    :initarg :socket-bundles
    :accessor socket-bundles)))

(defclass nest-implementation (p:fundamental-nest)
  ((%networking
    :initarg :networking
    :accessor networking)
   (%timing-wheel
    :initarg :timing-wheel
    :accessor timing-wheel)
   (%event-loop-queue
    :initarg :event-loop-queue
    :accessor event-loop-queue)
   (%event-loop-thread
    :initarg :event-loop-thread
    :accessor event-loop-thread))
  (:default-initargs
   :event-loop-queue (q:make-blocking-queue)))

(defun run-event-loop (nest)
  (iterate
    (with queue = (event-loop-queue nest))
    (for callback = (q:blocking-queue-pop! queue))
    (when nil
      (finish))
    (promise:fullfill! callback)))

(defmethod p:event-loop-schedule* ((nest nest-implementation) promise)
  (q:blocking-queue-push! (event-loop-queue nest)
                          promise)
  promise)

(defmethod p:timer-schedule* ((nest nest-implementation) delay promise)
  (tw:add! (timing-wheel nest) delay
           (lambda (tw) (declare (ignore tw))
             (p:event-loop-schedule* nest promise)))
  promise)
