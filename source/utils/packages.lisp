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
(cl:defpackage #:pantalea.utils.queue
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:make-queue
   #:make-blocking-queue
   #:queue-push/no-lock!
   #:queue-pop/no-lock!
   #:queue-push!
   #:lock
   #:queue-pop!
   #:blocking-queue-push!
   #:blocking-queue-pop!))

(cl:defpackage #:pantalea.utils.hashing
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:hash-key
   #:hash-vector))

(cl:defpackage #:pantalea.utils.hyperloglog
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:jaccard
   #:intersection-cardinality
   #:add-hash!
   #:add-key!
   #:hash-vector
   #:hash-key
   #:make-sketch))

(cl:defpackage #:pantalea.utils.bloom-filter
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:jaccard
   #:add-hash!
   #:add-key!
   #:make-sketch))

(cl:defpackage #:pantalea.utils.skip-list
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:drop!
   #:insert!
   #:make-skip-list))

(cl:defpackage #:pantalea.utils.dependency
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:dependency-cell
   #:on-dependency-killed
   #:dead-class
   #:kill
   #:deadp
   #:with-lock-held
   #:undepend
   #:spread
   #:depend))

(cl:defpackage #:pantalea.utils.timing-wheel
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames
   (#:q #:pantalea.utils.queue))
  (:export
   #:join-thread!
   #:run
   #:add!))

(cl:defpackage #:pantalea.utils.promise
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:fullfill!
   #:fullfilledp
   #:find-fullfilled
   #:attach-on-success!
   #:attach-on-failure!
   #:canceled
   #:*results*
   #:force-all!
   #:single-promise-success-p
   #:force!
   #:cancel!
   #:promise
   #:make-promise))

(cl:defpackage #:pantalea.utils.conditions
  (:use #:common-lisp #:pantalea.aux-package)
  (:local-nicknames)
  (:export
   #:stop-thread))
