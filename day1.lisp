(in-package :aoc-2024)

(defun parse-numbers ()
  (parse-lines (parse-list (parse-number) (parse-characters #\Space))))

(defun day1 (input &key (part 1))
  (let ((pairs (run-parser (parse-numbers) input)))
    (iter
      (for (a b) in pairs)
      (collect a into as)
      (collect b into bs)
      (finally
       (return (if (= part 1)
                   (iter
                     (for a in (sort as #'<))
                     (for b in (sort bs #'<))
                     (sum (abs (- a b))))
                   (iter
                     (for a in as)
                     (sum (* a (count-if (lambda (x) (= x a)) bs))))))))))
