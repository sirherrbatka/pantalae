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


(defclass future ()
  ((%lock
    :initarg :lock
    :accessor lock)
   (%cvar
    :initarg :cvar
    :accessor cvar)
   (%callback
    :initarg :callback
    :accessor callback)
   (%result
    :initarg :result
    :accessor result)
   (%completed
    :initarg :completed
    :reader p:completedp
    :accessor completed)))

(defmethod p:force ((future future))
  (bind (((:accessors lock cvar result completed) future))
    (bt:with-lock-held (lock)
      (iterate
        (until completed)
        (bt:condition-wait cvar lock)
        (finally (return-from p:force result))))))

(defmethod p:completedp ((future future))
  (bt:with-lock-held ((lock future))
    (completed future)))

(defun p:fullfill (future)
  (bind (((:accessors lock cvar callback result completed) future))
    (bt:with-lock-held (lock)
      (when completed
        (return-from p:fullfill result))
      (setf result (funcall callback)
            completed t)
      (bt:condition-notify cvar)
      result)))

(defclass networking ()
  ((%server-socket
    :initarg :server-socket
    :accessor server-socket)))

(defclass nest-implementation (p:fundamental-nest)
  ((%networking
    :initarg :networking
    :accessor networking)
   (%timing-wheel
    :initarg :timing-wheel
    :accessor timing-wheel)
   (%event-loop-queue
    :initarg :event-loop-queue
    :accessor event-loop-queue))
  (:default-initargs
   :event-loop-queue (q:make-blocking-queue)))

(defun run-event-loop (nest)
  (iterate
    (with queue = (event-loop-queue nest))
    (for callback = (q:blocking-queue-pop! queue))
    (when nil
      (finish))
    (p:fullfill callback)))

(defmethod p:event-loop-schedule* ((nest nest-implementation) promise)
  (q:blocking-queue-push! (event-loop-queue nest)
                          promise)
  promise)

(defmethod p:timer-schedule* ((nest nest-implementation) delay promise)
  (tw:add! (timing-wheel nest) delay
           (lambda (tw) (declare (ignore tw))
             (p:event-loop-schedule* nest promise)))
  promise)

(defmethod p:make-promise* ((nest nest-implementation) callback)
  (make 'future
        :cvar (bt:make-condition-variable)
        :lock (bt:make-lock)
        :result nil
        :completed nil))
