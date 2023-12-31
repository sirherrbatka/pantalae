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

(defclass networking ()
  ((%socket-bundles
    :initarg :socket-bundles
    :accessor socket-bundles)
   (%lock :initarg :lock
          :reader lock))
  (:default-initargs
   :lock (bt:make-lock)
   :socket-bundles (vect)))

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
    :accessor event-loop-thread)
   (%started
    :initarg :started
    :accessor started))
  (:default-initargs
   :started nil
   :timing-wheel nil
   :event-loop-thread nil
   :networking (make 'networking)
   :event-loop-queue (q:make-blocking-queue)))

(defclass ip-destination (p:fundamental-network-destination)
  ((%host
    :initarg :host
    :reader host))
  (:default-initargs
   ))
