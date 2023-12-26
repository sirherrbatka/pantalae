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
(cl:in-package #:pantalea.utils.timing-wheel)

(defclass queue ()
  ((%head
    :initarg :head
    :accessor head)
   (%lock
    :initarg :lock
    :accessor lock)
   (%tail
    :initarg :tail
    :accessor tail)))

(defun make-queue ()
  (make 'queue :lock (bt:make-lock)
               :head nil
               :tail nil))

(defun queue-push/no-lock! (queue value)
  (let ((new (cons value nil)))
      (if (head queue)
          (setf (cdr (tail queue)) new)
          (setf (head queue) new))
      (setf (tail queue) new))
  nil)

(defun queue-push! (queue value)
  (bt:with-lock-held ((lock queue))
    (queue-push/no-lock! queue value))
  nil)

(defun queue-pop! (queue)
  (bt:with-lock-held ((lock queue))
    (let ((node (head queue)))
      (if node
          (prog1 (car node)
            (when (null (setf (head queue) (cdr node)))
              (setf (tail queue) nil))
            (setf (car node) nil
                  (cdr node) nil))
          nil))))

(defclass timing-wheel ()
  ((%tick-duration
    :initarg :tick-duration
    :documentation "Tick duration in ms"
    :accessor tick-duration)
   (%buckets
    :initarg :buckets
    :accessor buckets)
   (%lock
    :initform (bt:make-lock)
    :accessor lock)
   (%wheel-pointer
    :initform 0
    :accessor wheel-pointer)
   (%thread
    :initarg :thread
    :initform nil
    :accessor thread)))

(defun make-timing-wheel (size tick-duration)
  (assert (> size 0))
  (assert (> tick-duration 0))
  (make 'timing-wheel
        :tick-duration tick-duration
        :buckets (map-into (make-array size) #'make-queue)))

(defstruct task
  callback
  (remaining-rounds 0 :type fixnum))

(defun task-run! (task)
  (funcall (task-callback task)))

(defun tick! (timing-wheel)
  (declare (optimize (speed 3) (safety 0)))
  (bind ((buckets (buckets timing-wheel))
         (wheel-pointer (wheel-pointer timing-wheel))
         (bucket (svref buckets wheel-pointer))
         (pending-tasks (list)))
    (declare (type simple-vector buckets)
             (type list pending-tasks)
             (type fixnum wheel-pointer))
    (iterate
      (for task = (queue-pop! bucket))
      (until (null task))
      (if (<= (task-remaining-rounds task) 0)
          (task-run! task)
          (progn
            (decf (task-remaining-rounds task))
            (push task pending-tasks))))
    (bt:with-lock-held ((lock bucket))
      (iterate
        (for task in pending-tasks)
        (queue-push/no-lock! bucket task)))
    (bt:with-lock-held ((lock timing-wheel))
      (setf (wheel-pointer timing-wheel)
            (mod (1+ wheel-pointer) (length buckets))))))

(defun run (size tick-duration)
  (lret ((timing-wheel (make-timing-wheel size tick-duration)))
    (let ((sleep-duration (/ tick-duration 1000.0)))
      (setf (thread timing-wheel)
            (bt:make-thread (lambda ()
                              (iterate
                                (tick! timing-wheel)
                                (sleep sleep-duration)))
                            :name "Timer wheel thread")))))

(defun add! (timing-wheel delay callback)
  (bind ((tick-duration (tick-duration timing-wheel))
         (ticks (round (+ delay tick-duration) tick-duration))
         (buckets (buckets timing-wheel))
         (size (length buckets))
         (pointer (mod (+ (bt:with-lock-held ((lock timing-wheel))
                            (wheel-pointer timing-wheel))
                          ticks)
                       size)))
    (declare (type simple-vector buckets))
    (queue-push! (aref buckets pointer)
                 (make-task :callback callback
                            :remaining-rounds (truncate ticks size)))))
