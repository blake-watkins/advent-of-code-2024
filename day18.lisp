(in-package :aoc-2024)

(defun parse-bytes ()
  (parse-lines
   (bind (parse-number-list) (lambda (xy) (unit (reverse xy))))))

(defparameter *dirs* '((-1 0) (1 0) (0 -1) (0 1)))

(defun get-bytes (bytes)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for step from 0)
    (for byte in bytes)
    (setf (gethash byte ret) step)
    (finally (return ret))))

(defun neighbours (pos step bytes max-dim)
  (iter
    (for dir in *dirs*)
    (for test = (point+ pos dir))
    (for in-range = (every (lambda (c d) (<= 0 c d)) test max-dim))
    (for in-time = (or (not (gethash test bytes))
                       (<= step (gethash test bytes))))
    (when (and in-range in-time)
      (collect test))))

(defun day18 (input steps max-dim)
  (declare (optimize (debug 3)))
  (let ((bytes (get-bytes (run-parser (parse-bytes) input))))
    (iter
      (for (vertex nil distance) in-dijkstra-from '(0 0) neighbours
           (lambda (v)
             (mapcar (lambda (p) (list p 1))
                     (neighbours v steps bytes max-dim))))
      (until (equal vertex max-dim))
      (finally (return (if (equal vertex max-dim) distance nil))))))
