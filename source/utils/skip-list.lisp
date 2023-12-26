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
(cl:in-package #:pantalea.utils.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type (unsigned-byte 64)))

(-> skip-list-node-level (skip-list-node) fixnum)
(defun skip-list-node-level (skip-list-node)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (~> skip-list-node skip-list-node-pointers length))

(-> skip-list-node-at (skip-list-node cl-ds.utils:index) t)
(defun skip-list-node-at (skip-list-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (~> skip-list-node skip-list-node-pointers (aref index)))

(-> (setf skip-list-node-at)
    ((or null skip-list-node) skip-list-node cl-ds.utils:index)
    (or null skip-list-node))
(defun (setf skip-list-node-at) (new-value skip-list-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (setf (aref (skip-list-node-pointers skip-list-node) index) new-value))

(defmethod print-object ((object skip-list-node) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "[~a:~a]"
            (skip-list-node-content object)
            (skip-list-node-value object))
    (print-object (skip-list-node-at object 0) stream)))

(-> copy-into! (simple-vector simple-vector &optional fixnum fixnum) simple-vector)
(declaim (inline copy-into!))
(defun copy-into! (destination source
                   &optional
                     (limit (min (length (the simple-vector destination))
                                 (length (the simple-vector source))))
                     (start 0))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum limit))
  (iterate
    (declare (type fixnum i))
    (for i from start below limit)
    (setf (aref destination i) (aref source i))
    (finally (return destination))))

(-> skip-list-node-update-pointers! (skip-list-node simple-vector) skip-list-node)
(defun skip-list-node-update-pointers! (skip-list-node new-pointers)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (copy-into! (skip-list-node-pointers skip-list-node) new-pointers)
  skip-list-node)

(-> skip-list-node-compare (function (or null skip-list-node) (or null skip-list-node)) boolean)
(declaim (inline skip-list-node-compare))
(defun skip-list-node-compare (test node1 node2)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (cl-ds.utils:cond+ ((null node1) (null node2))
    ((t t) nil)
    ((nil t) t)
    ((t nil) nil)
    ((nil nil) (funcall test
                        (skip-list-node-content node1)
                        (skip-list-node-content node2)))))

(-> new-node-update-pointers! (function skip-list-node simple-vector) skip-list-node)
(defun new-node-update-pointers! (test spliced-node pointers)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (iterate
    (declare (type fixnum i))
    (with spliced-level = (skip-list-node-level spliced-node))
    (for i from 0 below (length pointers))
    (for rest = (aref pointers i))
    (when (null rest)
      (next-iteration))
    (iterate
      (declare (type fixnum j))
      (with pointers = (skip-list-node-pointers spliced-node))
      (with level = (skip-list-node-level spliced-node))
      (for j from (the fixnum (1- (min level spliced-level))) downto 0)
      (if (skip-list-node-compare test
                                  spliced-node
                                  (aref pointers j))
          (setf (aref pointers j) spliced-node)
          (finish)))
    (finally (return spliced-node))))

(-> random-level (positive-fixnum) positive-fixnum)
(defun random-level (maximum-level)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iterate
    (declare (type fixnum i))
    (for i from 1 to maximum-level)
    (until (zerop (random 2)))
    (finally (return i))))

(-> make-skip-list-node-of-level (fixnum) skip-list-node)
(defun make-skip-list-node-of-level (level)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-skip-list-node :pointers (make-array level :initial-element nil)))

(declaim (notinline locate-node))
(-> locate-node (simple-vector t function) (values simple-vector
                                                   simple-vector))
(defun locate-node (pointers item test)
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     (compilation-speed 0) (space 0)))
  (let* ((pointers-length (length pointers))
         (prev-result (make-array pointers-length
                                  :initial-element nil))
         (last (1- pointers-length)))
    (iterate
      (declare (type fixnum i)
               (type simple-vector result))
      (with result = (copy-array pointers))
      (with i = last)
      (for node = (aref result i))
      (when (and node (funcall test (skip-list-node-content node) item))
        (copy-into! prev-result result (skip-list-node-level node))
        (copy-into! result (skip-list-node-pointers node))
        (setf i (skip-list-node-level node)))
      (decf i)
      (while (>= i 0))
      (finally (return (values result prev-result))))))

(defun gather-pointers (node pointers)
  (setf (aref pointers 0) node)
  (iterate
    (with level = (length pointers))
    (with current-level = 0)
    (while (< current-level level))
    (until (null node))
    (for node-level = (skip-list-node-level node))
    (iterate
      (for i from (1+ current-level) below node-level)
      (for next = (skip-list-node-at node i))
      (setf (aref pointers i) next))
    (when (> node-level (1+ current-level))
      (setf current-level node-level))
    (setf node (iterate
                 (for i from (1- node-level) downto 0)
                 (for next = (skip-list-node-at node i))
                 (finding next such-that (not (null next)))))
    (finally (return pointers))))

(-> insert-node-between! (simple-vector simple-vector function skip-list-node) skip-list-node)
(defun insert-node-between! (pointers previous-pointers test skip-list-node)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (new-node-update-pointers! test skip-list-node previous-pointers)
  (skip-list-node-update-pointers! skip-list-node pointers)
  skip-list-node)

(-> delete-node-between! (simple-vector simple-vector) skip-list-node)
(defun delete-node-between! (pointers prev-pointers)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (lret ((result (aref pointers 0)))
    (assert (not (null result)))
    (iterate
      (declare (type fixnum i next-size))
      (with next = (skip-list-node-pointers result))
      (with next-size = (length next))
      (for i from 0 below next-size)
      (for node = (aref prev-pointers i))
      (copy-into! (skip-list-node-pointers node)
                  next))))

(-> make-skip-list-node-of-random-level (fixnum) skip-list-node)
(defun make-skip-list-node-of-random-level (maximum-level)
  (make-skip-list-node-of-level (random-level maximum-level)))

(defclass skip-list ()
  ((%ordering-function :initarg :ordering-function
                       :reader read-ordering-function)
   (%test-function :initarg :test-function
                   :accessor access-test-function)
   (%pointers :initarg :pointers
              :reader read-pointers
              :type simple-vector)
   (%maximum-level :initarg :maximum-level
                   :accessor access-maximum-level))
  (:default-initargs
   :maximum-level 32))

(defun make-skip-list (ordering-function test-function maximum-level)
  (make 'skip-list
        :ordering-function (ensure-function ordering-function)
        :test-function (ensure-function test-function)
        :pointers (make-array maximum-level :initial-element nil)
        :maximum-level maximum-level))

(-> skip-list-locate-node (skip-list t) (values simple-vector simple-vector))
(defun skip-list-locate-node (skip-list item)
  (locate-node (read-pointers skip-list)
               item
               (read-ordering-function skip-list)))

(defun skip-list-update-head-pointers-after-insert! (skip-list skip-list-node)
  (declare (type skip-list-node skip-list-node)
           (type skip-list skip-list)
           (optimize (speed 3) (debug 0) (safety 0)))
  (iterate
    (declare (type fixnum i)
             (type simple-vector head))
    (with head = (read-pointers skip-list))
    (with content = (skip-list-node-content skip-list-node))
    (with ordering-function = (ensure-function (read-ordering-function skip-list)))
    (for i from (~> skip-list-node skip-list-node-level 1-) downto 0)
    (for node = (aref head i))
    (when (null node)
      (setf (aref head i) skip-list-node)
      (next-iteration))
    (for old-content = (skip-list-node-content node))
    (for should-go-before = (funcall ordering-function content old-content))
    (if should-go-before
        (setf (aref head i) skip-list-node)
        (finish))))

(defun insert! (structure location)
  (bind ((pointers (read-pointers structure))
         (test (read-ordering-function structure))
         ((:values current prev)
          (locate-node pointers location test))
         (result (aref current 0)))
    (when (null result)
      (let ((new-node (make-skip-list-node-of-random-level (access-maximum-level structure))))
        (insert-node-between!
         current prev
         (read-ordering-function structure)
         new-node)
        (skip-list-update-head-pointers-after-insert! structure new-node)
        (return-from skip-list-insert! t)))
    (bind ((content (skip-list-node-content result))
           (found (~> structure cl-ds.common.skip-list:access-test-function
                      (funcall content location))))
      (if found
          nil
          (let ((new-node (make-skip-list-node-of-random-level (access-maximum-level structure))))
            (insert-node-between! current prev test new-node)
            (skip-list-update-head-pointers-after-insert! structure new-node)
            t)))))

(defun skip-list-node-find-node-of-level (node level)
  (tagbody start
     (if (>= (skip-list-node-level node) level)
         (return-from skip-list-node-find-node-of-level node)
         (iterate
           (with pointers = (skip-list-node-pointers node))
           (for i from (1- (skip-list-node-level node)) downto 0)
           (for next = (aref pointers i))
           (unless (null next)
             (setf node next)
             (go start))))))

(defun drop! (structure location)
  (bind ((pointers (read-pointers structure))
         (test (read-ordering-function structure))
         (current (locate-node pointers location test))
         (maximum-level (access-maximum-level structure))
         (result (aref current 0)))
    (when (null result)
      (return-from skip-list-drop! nil))
    (copy-into! pointers current)
    (iterate
      (with n = result)
      (for i from 0 below maximum-level)
      (setf n (skip-list-node-find-node-of-level n i))
      (setf (aref pointers i) n))
    t))
