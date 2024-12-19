(in-package :aoc-2024)

(defun test (a b c)
  (iter
    (setf b (mod a 8))
    (setf b (logxor b 2))
    (setf c (floor a (expt 2 b)))
    (setf b (logxor b 7))
    (setf b (logxor b c))
    (setf a (floor a (expt 2 3)))
    (collect (mod b 8))
    (until (= 0 a))))

(defun find-match (acc target)
  (declare (optimize (debug 3)))
  (if (= (length acc) (length target))
      (if (equal (test (digits-to-int (reverse acc) :base 8) 0 0) target)
          acc nil)
      (iter
        (for a below 8)
        (for test-val = (cons a acc))
        (for test-output =
             (test (digits-to-int (reverse test-val) :base 8) 0 0))
        (for compare =
             (subseq target (- (length target) (length test-output))))
        (when (equal test-output compare)
          (thereis (find-match test-val target))))))
