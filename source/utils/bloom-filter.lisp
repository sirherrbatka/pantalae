(cl:in-package #:pantalea.utils.bloom-filter)


(defvar *hashes* #2A((3864772122 2076398090)
                     (519963632 1761369836)
                     (2600040084 317442929)
                     (1245453434 561978320)
                     (3849033209 1651543515)
                     (383175112 2619333402)
                     (4142397413 3674008792)
                     (3834207618 811637147)))

(defvar *depth* 65536)

(defvar *width* 8)

(defconstant +long-prime+ 4294967311)

(defun make-sketch ()
  (make-array *depth* :initial-element 0 :element-type 'bit))

(defun jaccard (a-counters b-counters)
  (iterate
    (for i from 0 below (array-total-size a-counters))
    (counting (= 1
                 (row-major-aref a-counters i)
                 (row-major-aref b-counters i))
              into intersection)
    (counting (or (= 1 (row-major-aref a-counters i))
                  (= 1 (row-major-aref b-counters i)))
              into union)
    (finally (return (- 1.0 (/ intersection union))))))

(defun hashval-no-depth (hashes j hash)
  (declare (type non-negative-fixnum j hash))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 32 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 32 0) _)
      (rem +long-prime+)))

(defun hashval (hashes depth j hash)
  (declare (type non-negative-fixnum depth j hash))
  (~> (hashval-no-depth hashes j hash)
      (mod depth)))

(defun add-hash! (counters hash)
  (setf hash (ldb (byte 32 0) hash))
  (iterate
    (for j from 0 below *width*)
    (setf (aref counters (hashval *hashes* *depth* j hash)) 1)))

(defun add-key! (sketch key)
  (add-hash! sketch (pantalea.utils.hashing:hash-key key)))
