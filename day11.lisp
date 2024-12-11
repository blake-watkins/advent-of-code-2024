(in-package :aoc-2024)

(defun digits-in-int (i)
  (if (< i 10) 1 (1+ (digits-in-int (floor i 10)))))

(defun evolve (stone)
  (cond
    ((= stone 0) '(1))
    ((= 0 (mod (digits-in-int stone) 2))
     (let ((digits (/ (digits-in-int stone) 2)))
       (multiple-value-list (floor stone (expt 10 digits)))))
    (t (list (* stone 2024)))))

(defun num-stones (stone evolutions dp)
  (let ((stored (gethash (list stone evolutions) dp)))
    (cond
      (stored stored)
      ((= 0 evolutions) 1)
      (t (let* ((stones (evolve stone))
                (num-stones
                  (iter
                    (for next-stone in stones)
                    (sum (num-stones next-stone (1- evolutions) dp)))))
           (setf (gethash (list stone evolutions) dp) num-stones))))))

(defun day11 (input &key (part 1))
  (let ((stones (run-parser (parse-number-list #\Space) input)))
    (iter
      (for stone in stones)
      (sum (num-stones stone
                       (if (= part 1) 25 75)
                       (make-hash-table :test 'equal))))))
