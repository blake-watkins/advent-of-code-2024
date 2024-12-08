(in-package :aoc-2024)

(defun parse-updates ()
  (with-monad
    (assign rules (parse-lines (parse-list (parse-number) #\|)))
    (parse-newline)
    (parse-newline)
    (assign updates (parse-lines (parse-number-list)))
    (unit (list rules updates))))

(defun test-update (comes-before update)
  (iter
    (for a on update)
    (for cur = (first a))
    (always (iter
              (for after in (rest a))
              (never (find (list after cur) comes-before :test 'equal))))))

(defun reorder (comes-before update)
  (sort update (lambda (a b) (find (list a b) comes-before :test 'equal))))

(defun day5 (input &key (part 1))
  (destructuring-bind (comes-before updates) (run-parser (parse-updates) input)
    (iter
      (for update in updates)
      (for midpoint = (floor (length update) 2))
      (if (= part 1)
          (when (test-update comes-before update)
            (sum (elt update midpoint)))
          (unless (test-update comes-before update)
            (sum (elt (reorder comes-before update) midpoint)))))))
