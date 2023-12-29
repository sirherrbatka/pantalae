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
(cl:in-package #:pantalea.utils.promise)


(defclass promise ()
  ())

(defclass single-promise (promise)
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
   (%fullfilled
    :initarg :fullfilled
    :accessor fullfilled)))

(defclass combined-promise (promise)
  ((%content
    :initarg :content
    :accessor content)))

(defgeneric force! (promise)
  (:method ((promise t))
    promise))

(defmethod force! ((promise single-promise))
  (bind (((:accessors lock cvar result fullfilled) promise))
    (bt:with-lock-held (lock)
      (iterate
        (until fullfilled)
        (bt:condition-wait cvar lock)
        (finally (return-from force! result))))))

(defmethod force! ((promise combined-promise))
  (map 'list #'force! (content promise)))

(defgeneric fullfilledp (promise)
  (:method ((promise t))
    t))

(defmethod fullfilledp ((promise single-promise))
  (bt:with-lock-held ((lock promise))
    (fullfilled promise)))

(defmethod fullfilledp ((promise combined-promise))
  (every #'fullfilledp (content promise)))

(defgeneric fullfill! (promise))

(defmethod fullfill! ((promise single-promise))
  (bind (((:accessors lock cvar callback result fullfilled) promise))
    (unwind-protect
         (bt:with-lock-held (lock)
           (unless fullfilled
             (handler-case
                 (setf fullfilled t
                       result (funcall callback))
               (t (s)
                 (setf result s)
                 (signal s))))
           result)
      (bt:condition-notify cvar))))

(defmethod fullfill! ((promise combined-promise))
  (map nil #'fullfill! (content promise)))

(defun make-promise (callback)
  (make 'single-promise
        :cvar (bt:make-condition-variable)
        :callback callback
        :lock (bt:make-lock)
        :result nil
        :fullfilled nil))

(defmacro promise (&body body)
  `(make-promise (lambda () ,@body)))

(defun combine (promises)
  (check-type promises sequence)
  (make 'combined-promise
        :content promises))
