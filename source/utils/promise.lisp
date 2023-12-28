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

(defun force! (promise)
  (if (typep promise 'promise)
      (bind (((:accessors lock cvar result fullfilled) promise))
        (bt:with-lock-held (lock)
          (iterate
            (until fullfilled)
            (bt:condition-wait cvar lock)
            (finally (return-from force! result)))))
      promise))

(defun fullfilledp (promise)
  (if (typep promise 'promise)
      (bt:with-lock-held ((lock promise))
        (fullfilledp promise))
      t))

(defun fullfill! (promise)
  (bind (((:accessors lock cvar callback result fullfilled) promise))
    (bt:with-lock-held (lock)
      (when fullfilled
        (return-from fullfill! result))
      (setf result (funcall callback)
            fullfilled t)
      (bt:condition-notify cvar)
      result)))

(defun make-promise (callback)
  (make 'promise
        :cvar (bt:make-condition-variable)
        :callback callback
        :lock (bt:make-lock)
        :result nil
        :completed nil))

(defmacro promise (&body body)
  `(make-promise (lambda () ,@body)))
