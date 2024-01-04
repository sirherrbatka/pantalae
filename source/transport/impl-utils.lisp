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

(defun send-ping (connection)
  (p:send-packet connection p:+type-ping+ +empty-packet+))

(defun send-pong (connection)
  (p:send-packet connection p:+type-pong+ +empty-packet+))

(defun schedule-ping (nest connection)
  (flet ((pinging ()
           (if (send-ping connection)
               (setf (p:ping-at connection) (local-time:now))
               (log4cl:log-warn "Ping not sent!"))))
    (log4cl:log-debug "Scheduling ping!")
    (p:schedule-to-event-loop* nest
                               #'pinging
                               +ping-delay+)))
