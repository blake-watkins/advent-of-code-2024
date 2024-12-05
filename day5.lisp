(in-package :aoc-2024)

(defun parse-updates ()
  (with-monad
    (assign rules (parse-lines (parse-list (parse-number) #\|)))
    (parse-newline)
    (parse-newline)
    (assign updates (parse-lines (parse-number-list)))
    (unit (list rules updates))))

(defun comes-before (rules)
  "Get a hash table of all values that must come before each key value."
  (iter
    (with ret = (make-hash-table))
    (for (a b) in rules)
    (push a (gethash b ret '()))
    (finally (return ret))))

(defun test-update (comes-before update)
  (iter
    (for a on update)
    (for cur = (first a))
    (always (iter
              (for after in (rest a))
              (never (member after (gethash cur comes-before)))))))

(defun reorder (comes-before update)
  (if (<= (length update) 1)
      update
      (let ((a (first update))
            (rest-ordered (reorder comes-before (cdr update))))
        (iter
          (for i index-of-sequence rest-ordered)
          (for after in rest-ordered)
          (until (member a (gethash after comes-before)))
          (finally (return (concatenate 'list
                                        (subseq rest-ordered 0 i)
                                        (list a)
                                        (subseq rest-ordered i))))))))
(defun day5 (input &key (part 1))
  (destructuring-bind (rules updates) (run-parser (parse-updates) input)
    (iter
      (with comes-before = (comes-before rules))
      (for update in updates)
      (for midpoint = (floor (length update) 2))
      (if (= part 1)
          (when (test-update comes-before update)
            (sum (elt update midpoint)))
          (unless (test-update comes-before update)
            (sum (elt (reorder comes-before update) midpoint)))))))
