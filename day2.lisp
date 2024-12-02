(in-package :aoc-2024)

(defun is-safe (levels)
  (or (<= (length levels) 1)
      (iter
        (with increasing = (> (second levels) (first levels)))
        (while (>= (length levels) 2))
        (for diff = (- (second levels) (first levels)))
        (always (<= 1 (if increasing diff (- diff)) 3))
        (setf levels (rest levels)))))

(defun is-dampened-safe (levels)
  (iter
    (for i below (length levels))
    (for dampened =
         (concatenate 'list (subseq levels 0 i) (subseq levels (1+ i))))
    (thereis (is-safe dampened))))

(defun day2 (input &key (part 1))
  (iter
    (for levels in (run-parser (parse-lines (parse-number-list #\Space)) input))
    (counting (if (= part 1) (is-safe levels) (is-dampened-safe levels)))))
