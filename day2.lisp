(in-package :aoc-2024)

(defun parse-numbers ()
  (parse-lines (parse-number-list #\Space)))

(defun safe (levels)
  (or (<= (length levels) 1)
      (iter
        (with increasing = (> (second levels) (first levels)))
        (while (>= (length levels) 2))
        (for diff = (- (second levels) (first levels)))
        (always (if increasing
                    (<= 1 diff 3)
                    (<= -3 diff -1)))
        (setf levels (rest levels)))))

(defun day2 (input &key (part 1))
  (iter
    (for levels in (run-parser (parse-numbers) input))
    (counting (safe levels))))
