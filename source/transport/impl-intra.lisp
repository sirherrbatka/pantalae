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
(cl:in-package #:pantalea.transport.intra)


(defclass networking ()
  ((%connections
    :initarg :connections
    :reader connections)
   (%incoming-queue
    :initarg :incoming-queue
    :reader incoming-queue)
   (%lock :initarg :lock
          :reader lock))
  (:default-initargs
   :lock (bt:make-lock "Connections lock")
   :connections (vect)))

(defun intra-networking (nest)
  (p:networking nest :intra))

(defclass destination (p:fundamental-network-destination)
  ((%other-nest
    :initarg :other-nest
    :accessor other-nest)))

(defclass connection (p:fundamental-connection)
  ((%incoming-queue
    :initarg :incoming-queue
    :accessor incoming-queue)
   (%outgoing-queue
    :initarg :outgoing-queue
    :accessor outgoing-queue)
   (%thread
    :initarg :thread
    :accessor thread)
   (%other-nest
    :initarg :other-nest
    :accessor other-nest))
  (:default-initargs
   :thread nil
   :incoming-queue (q:make-blocking-queue)
   :outgoing-queue (q:make-blocking-queue)))

(defmethod p:send-packet ((connection connection) type packet)
  (q:blocking-queue-push! (outgoing-queue connection) (cons type packet))
  t)

(defmethod p:disconnected ((nest p:fundamental-nest) (connection connection) reason)
  (log4cl:log-info "Connection to ~a lost because ~a." (other-nest connection) reason)
  (bt:with-lock-held ((~> nest intra-networking lock))
    (let* ((connections (~> nest intra-networking connections))
           (last-index (~> connections length 1-))
           (index (iterate
                    (for i from 0 below (length connections))
                    (for c = (aref connections i))
                    (finding i such-that (eq (other-nest c)
                                             (other-nest connection))))))
      (when (null index)
        (log4cl:log-warn "Connection to ~a was not found in nest!" connection)
        (return-from p:disconnected nil))
      (rotatef (aref connections index) (aref connections last-index))
      (setf (aref connections last-index) nil)
      (decf (fill-pointer connections)))))

(defmethod p:disconnect* ((nest p:fundamental-nest) (connection connection))
  (let ((promise (promise:promise t)))
    (q:blocking-queue-push! (incoming-queue connection)
                            promise)
    nil))

(defmethod p:stop-networking ((nest p:fundamental-nest) (networking networking))
  (log4cl:log-info "Stopping networking.")
  (~> networking
      connections
      (map 'list (lambda (connection)
                   (lret ((promise (promise:promise t)))
                     (q:blocking-queue-push! (incoming-queue connection)
                                             promise)))
           _)
      promise:combine
      (p:schedule-to-event-loop/no-lock nest _)
      promise:force!)
  networking)

(defmethod p:start-networking ((nest p:fundamental-nest) (networking networking))
  (log4cl:log-info "Starting networking.")
  networking)

(defun run-connection (connection nest promise)
  (setf (thread connection)
        (bt:make-thread
         (lambda (&aux (end nil))
           (unwind-protect
                (bind ((local-client (p:make-double-ratchet-local-client nest))
                       (incoming-queue (incoming-queue connection))
                       ((:flet read-connection ())
                        (let ((elt (q:blocking-queue-pop! incoming-queue)))
                          (if (consp elt)
                              (return-from read-connection (values (car elt) (cdr elt)))
                              (progn
                                (setf end elt)
                                (error "Ending thread!")
                                )))))
                  (log4cl:log-info "Starting connection thread.")
                  (p:send-keys connection local-client)
                  (iterate
                    (for (values type packet) = (read-connection))
                    (when (= type p:+type-keys+)
                      (p:set-double-ratchet connection local-client (conspack:decode packet))
                      (leave)))
                  (when promise
                    (p:schedule-to-event-loop* nest promise))
                  (iterate
                    (for (values type packet) = (read-connection))
                    (p:schedule-to-event-loop* nest
                                               (curry #'p:handle-incoming-packet*
                                                      nest
                                                      connection
                                                      type
                                                      packet))))
             (~> connection outgoing-queue (q:blocking-queue-push! (promise:promise nil)))
             (p:schedule-to-event-loop* nest (curry #'p:disconnected nest connection nil))
             (when end
               (promise:fullfill! end)))))))

(defun make-connection (destination-nest origin-nest)
  (bt:with-lock-held ((~> destination-nest intra-networking lock))
    (let ((queue-a (q:make-blocking-queue))
          (queue-b (q:make-blocking-queue)))
      (vector-push-extend (make 'connection
                                :other-nest origin-nest
                                :incoming-queue queue-a
                                :outgoing-queue queue-b)
                          (~> destination-nest intra-networking connections))
      (~> destination-nest intra-networking
          connections last-elt
          (run-connection destination-nest
                          (curry #'p:connected destination-nest
                                 (make 'destination
                                       :other-nest origin-nest)
                                 (~> destination-nest intra-networking connections last-elt))))
      (make 'connection
            :other-nest destination-nest
            :incoming-queue queue-b
            :outgoing-queue queue-a))))

(defmethod p:connect* ((nest p:fundamental-nest) (destination destination))
  (bt:with-lock-held ((~> nest intra-networking lock))
    (if-let ((existing
              (iterate
                (for connection in-vector (~> nest intra-networking connections))
                (finding connection such-that (eq (other-nest connection) (other-nest destination))))))
      (progn
        (log4cl:log-info "Using existing connection.")
        existing)
      (let ((connection (~> destination other-nest (make-connection nest))))
        (vector-push-extend connection (~> nest intra-networking connections))
        (~> nest intra-networking connections
            last-elt (run-connection nest (curry #'p:connected nest destination connection)))
        connection))))

(defmethod p:map-connections ((networking networking) function)
  (map nil function (connections networking)))
