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
(cl:in-package #:pantalea.transport.protocol)


(defmethod start-nest* :around ((nest fundamental-nest))
  (bt:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod stop-nest* :around ((nest fundamental-nest))
  (bt:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod connect* :around ((nest fundamental-nest) destination)
  (bt:with-lock-held ((main-nest-lock nest))
    (call-next-method)))

(defmethod schedule-to-event-loop* ((nest fundamental-nest) promise &optional (delay 0))
  (bt:with-lock-held ((main-nest-lock nest))
    (schedule-to-event-loop/no-lock nest promise delay)))

(defmethod disconnected :after ((nest fundamental-nest) (connection fundamental-connection) reason)
  (pantalea.utils.dependency:kill connection))

(defmethod send-packet ((connection dead-connection) type packet)
  nil)

(defmethod send-packet :around ((connection fundamental-connection) type packet)
  (pantalea.utils.dependency:with-lock-held (connection)
    (call-next-method)))

(defmethod pantalea.utils.dependency:dead-class ((connection fundamental-connection))
  'dead-connection)

(defmethod disconnect* ((nest fundamental-nest) (connection dead-connection))
  nil)

(defmethod long-term-identity-remote-key ((connection fundamental-connection))
  (~> connection double-ratchet dr:long-term-identity-remote-key))

(defmethod insert-direct-route ((nest fundamental-nest) connection)
  (bind ((key (long-term-identity-remote-key connection))
         (routing-table (routing-table nest))
         (own-routes (own-routes routing-table))
         (container (ensure (gethash key own-routes)
                      (make 'route-container
                            :content nil)))
         (new-route (make 'direct-route :connection connection
                                        :destination-public-key key)))
    (pantalea.utils.dependency:depend new-route connection)
    (pantalea.utils.dependency:depend container new-route)
    (when-let ((old-route (content container)))
      (pantalea.utils.dependency:undepend container old-route))
    (setf (content container) new-route)
    nil))

(defmethod connection ((route route-container))
  (~> route content connection))

(defmethod destination-public-key ((route route-container))
  (~> route content destination-public-key))

(defmethod established-promise ((route route-container))
  (~> route content established-promise))

(defmethod timeout-promise ((route route-container))
  (~> route content timeout-promise))
