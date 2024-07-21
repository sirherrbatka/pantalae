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

(defvar *results*)

(defclass single-promise (promise)
  ((%lock
    :initarg :lock
    :initform (bt2:make-lock :name "PROMISE lock")
    :accessor lock)
   (%cvar
    :initarg :cvar
    :initform (bt2:make-condition-variable)
    :accessor cvar)
   (%canceled
    :initarg :canceld
    :initform nil
    :accessor canceled)
   (%callback
    :initarg :callback
    :accessor callback)
   (%result
    :initarg :result
    :accessor result)
   (%successp
    :initarg :successp
    :reader single-promise-success-p
    :accessor successp)
   (%success-hooks
    :initform nil
    :accessor success-hooks)
   (%failure-hooks
    :initform nil
    :accessor failure-hooks)
   (%fullfilled
    :initarg :fullfilled
    :accessor fullfilled)))

(defgeneric force! (promise &key timeout loop)
  (:method ((promise t) &key timeout loop)
    (declare (ignore timeout loop))
    (values promise t)))

(defmethod force! ((promise single-promise) &key timeout (loop t))
  (bind (((:accessors lock cvar result fullfilled) promise))
    (bt2:with-lock-held (lock)
      (if loop
          (iterate
            (until fullfilled)
            (bt2:condition-wait cvar lock :timeout timeout)
            (finally
             (if (typep result 'condition)
                 (signal result)
                 (return-from force! (values result fullfilled)))))
          (progn
            (unless (fullfilled promise)
              (bt2:condition-wait cvar lock :timeout timeout))
            (if (typep result 'condition)
                 (signal result)
                 (return-from force! (values result fullfilled))))))))

(defgeneric fullfilledp (promise)
  (:method ((promise t))
    t))

(defmethod fullfilledp ((promise single-promise))
  (bt2:with-lock-held ((lock promise))
    (fullfilled promise)))

(defgeneric fullfill! (promise))

(defmethod fullfill! ((promise single-promise))
  (bind (((:accessors lock cvar callback result fullfilled successp success-hooks failure-hooks) promise))
    (unwind-protect
         (bt2:with-lock-held (lock)
           (when fullfilled
             (return-from fullfill! result))
           (handler-case
               (setf fullfilled t
                     result (funcall callback)
                     successp t)
             (condition (s)
               (setf result s)
               (signal s)))
           result)
      (iterate
        (for hook in (if successp success-hooks failure-hooks))
        (ignore-errors (funcall hook result)))
      (bt2:condition-notify cvar))))

(defgeneric cancel! (promise &optional condition))

(define-condition canceled (error)
  ())

(defmethod cancel! ((promise single-promise) &optional (condition (make-condition 'canceled)))
  (bind (((:accessors lock cvar result failure-hooks fullfilled canceled) promise))
    (bt2:with-lock-held (lock)
      (unless fullfilled
        (setf result condition
              canceled t
              fullfilled t)
        (iterate
          (for hook in failure-hooks)
          (ignore-errors (funcall hook result)))))))

(defun make-promise (callback)
  (make 'single-promise
        :callback callback
        :result nil
        :successp nil
        :fullfilled nil))

(defgeneric attach-on-success!* (promise &rest callbacks))

(defgeneric attach-on-failure!* (promise &rest callbacks))

(defmethod attach-on-success!* ((promise single-promise) &rest callbacks)
  (check-type promise single-promise)
  (bind (((:accessors lock cvar result fullfilled success-hooks canceled successp) promise))
    (bt2:with-lock-held (lock)
      (if fullfilled
          (if successp
              (iterate
                (for callback in callbacks)
                (ignore-errors (funcall callback result)))
              nil)
          (iterate
            (for callback in callbacks)
            (push callback success-hooks)))
      promise)))

(defmethod attach-on-failure!* ((promise single-promise) &rest callbacks)
  (check-type promise single-promise)
  (bind (((:accessors lock cvar result fullfilled failure-hooks canceled successp) promise))
    (bt2:with-lock-held (lock)
      (if fullfilled
          (if successp
              nil
              (iterate
                (for callback in callbacks)
                (ignore-errors (funcall callback result))))
          (iterate
            (for callback in callbacks)
            (push callback failure-hooks)))
      promise)))

(defmacro attach-on-success! (promise variable &body hooks)
  `(attach-on-success!* ,promise
    ,@(mapcar (lambda (body) `(lambda (,variable) ,@body))
              hooks)))

(defmacro attach-on-failure! (promise variable &body hooks)
  `(attach-on-failure!* ,promise
    ,@(mapcar (lambda (body) `(lambda (,variable) ,@body))
              hooks)))

(defmacro promise (&body body)
  `(make-promise (lambda () ,@body)))

(defun find-fullfilled (promise &rest promises)
  (iterate
    (with all-promises = (cons promise promises))
    (iterate
      (for i from 0)
      (for promise in all-promises)
      (for (values v success) = (force! promise :loop nil :timeout 0.1))
      (when success (return-from find-fullfilled (values v i))))))

(defun force-all! (promises &key timeout (loop t))
  (map 'list
       (lambda (promise)
         (force! promise :timeout timeout :loop loop))
       promises))
