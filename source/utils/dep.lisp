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
(cl:in-package #:pantalea.utils.dependency)


(defclass dependency-cell ()
  ((%terminated-lock
    :initarg :terminated-lock
    :initform (bt:make-lock)
    :accessor terminated-lock)
   (%dependent-lock
    :initarg :dependent-lock
    :initform (bt:make-lock)
    :accessor dependent-lock)
   (%dependent
    :initarg :dependent
    :initform (make-hash-table :test 'eq :weakness :key-and-value)
    :accessor dependent)
   (%terminatedp
    :initarg :terminatedp
    :initform nil
    :accessor terminatedp))
  (:default-initargs))

(defgeneric dead-class (object)
  (:method ((cell dependency-cell))
    'dependency-cell))

(defun deadp (cell)
  (bind (((:accessors terminated-lock terminatedp) cell))
    (bt:with-lock-held (terminated-lock)
      terminatedp)))

(defgeneric on-dependency-killed (cell dependency)
  (:method ((cell dependency-cell) dependency)
    (kill cell)))

(defgeneric spread (cell event &rest all)
  (:method :around ((cell dependency-cell) event &rest all)
    (declare (ignore all))
    (bind (((:accessors dependent-lock terminated-lock terminatedp dependent) cell))
      (bt:with-lock-held (terminated-lock)
        (when terminatedp
          (return-from spread cell))
        (call-next-method)))
    cell)
  (:method ((cell dependency-cell) event &rest all)
    (bind (((:accessors dependent-lock terminated-lock terminatedp dependent) cell))
      (bt:with-lock-held (dependent-lock)
        (maphash-values (lambda (dep) (apply #'spread dep event all))
                        dependent)))))

(defgeneric kill (cell)
  (:method ((cell dependency-cell))
    (bind (((:accessors dependent-lock terminated-lock terminatedp dependent) cell))
      (bt:with-lock-held (terminated-lock)
        (when terminatedp
          (return-from kill cell))
        (bt:with-lock-held (dependent-lock)
          (maphash-values (lambda (dep) (on-dependency-killed dep cell))
                          dependent))
        (setf terminatedp t)
        (change-class cell (dead-class cell))))
    cell))

(defgeneric depend (dependent dependency)
  (:method ((a dependency-cell) (b dependency-cell))
    (bind (((:accessors terminated-lock dependent-lock dependent) b))
      (bt:with-lock-held (terminated-lock)
        (bt:with-lock-held (dependent-lock)
          (setf (gethash a dependent) a)))
      nil)))

(defgeneric undepend (dependent dependency)
  (:method ((a dependency-cell) (b dependency-cell))
    (bind (((:accessors terminated-lock dependent-lock dependent) b))
      (bt:with-lock-held (terminated-lock)
        (bt:with-lock-held (dependent-lock)
          (remhash a dependent)))
      nil)))

(defmacro with-lock-held ((cell) &body body)
  `(bt:with-lock-held ((terminated-lock ,cell))
     ,@body))
