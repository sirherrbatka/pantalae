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
    :initform (bt2:make-lock "PROMISE lock")
    :accessor lock)
   (%cvar
    :initarg :cvar
    :initform (bt2:make-condition-variable)
    :accessor cvar)
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
   (%fullfilled
    :initarg :fullfilled
    :accessor fullfilled)))

(defclass eager-promise (single-promise)
  ())

(defclass combined-promise (promise)
  ((%content
    :initarg :content
    :accessor content)))

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

(defmethod force! ((promise eager-promise) &key timeout (loop t))
  (declare (ignore timeout loop))
  (bind (((:accessors callback lock successp cvar result fullfilled) promise))
    (bt2:with-lock-held (lock)
      (unless fullfilled
        (handler-case
            (setf fullfilled t
                  result (funcall callback)
                  successp t)
          (condition (s)
            (setf result s))))
      (if (typep result 'condition)
          (signal result)
          (return-from force! (values result t))))))

(defmethod force! ((promise combined-promise) &key timeout (loop t))
  (map 'list
       (lambda (promise)
         (force! promise :timeout timeout :loop loop))
       (content promise)))

(defgeneric fullfilledp (promise)
  (:method ((promise t))
    t))

(defmethod fullfilledp ((promise single-promise))
  (bt2:with-lock-held ((lock promise))
    (fullfilled promise)))

(defmethod fullfilledp ((promise combined-promise))
  (every #'fullfilledp (content promise)))

(defgeneric fullfill! (promise &key value success))

(defmethod fullfill! ((promise single-promise) &key (value nil value-bound-p) (success t success-bound-p))
  (bind (((:accessors lock cvar callback result fullfilled successp) promise))
    (unwind-protect
         (bt2:with-lock-held (lock)
           (unless fullfilled
             (handler-case
                 (setf fullfilled t
                       result (if value-bound-p
                                  (progn (ignore-errors (funcall callback))
                                         value)
                                  (funcall callback))
                       successp (if success-bound-p success t))
               (condition (s)
                 (setf result s)
                 (signal s))))
           result)
      (bt2:condition-notify cvar))))

(defmethod fullfill! ((promise combined-promise) &key (value nil value-bound-p) (success t success-bound-p))
  (if value-bound-p
      (if success-bound-p
          (map nil
               (lambda (promise)
                 (fullfill! promise :value value :success success))
               (content promise))
          (map nil
               (lambda (promise)
                 (fullfill! promise :success success))
               (content promise)))
      (if success-bound-p
          (map nil
               (lambda (promise)
                 (fullfill! promise :success success))
               (content promise))
          (map nil
               (lambda (promise)
                 (fullfill! promise))
               (content promise)))))

(defgeneric cancel! (promise))

(defmethod cancel! ((promise single-promise))
  (bind (((:accessors lock cvar result fullfilled) promise))
    (bt2:with-lock-held (lock)
      (unless fullfilled
        (setf result nil
              fullfilled t)))))

(defun make-promise (callback)
  (make 'single-promise
        :callback callback
        :result nil
        :successp nil
        :fullfilled nil))

(defmacro promise (&body body)
  `(make-promise (lambda () ,@body)))

(defun combine (promises)
  (check-type promises sequence)
  (make 'combined-promise
        :content promises))

(defun find-fullfilled (promise &rest promises)
  (iterate
    (with all-promises = (cons promise promises))
    (iterate
      (for i from 0)
      (for promise in all-promises)
      (for (values v success) = (force! promise :loop nil :timeout 0.1))
      (when success (return-from find-fullfilled (values v i))))))

(defmacro eager-promise (&body body)
  `(make 'eager-promise
    :callback (lambda () ,@body)
    :result nil
    :successp nil
    :fullfilled nil))
