(in-package :aoc-2024)

(defun parse-color ()
  (bind (parse-character "rugbw")
        (lambda (c)
          (unit (intern (string-upcase c) :keyword)))))

(defun parse-towels ()
  (parse-list (one-or-more (parse-color)) ", "))

(defun parse-all ()
  (with-monad
    (assign towels (parse-towels))
    (parse-newline)
    (parse-newline)
    (assign designs (parse-lines (one-or-more (parse-color))))
    (unit (list towels designs))))

(defun match-design (design towel)
  (if (and (<= (length towel) (length design))
           (every #'eq towel design))
      (values t (subseq design (length towel)))
      (values nil design)))

(defun match-full-design (design towels dp)
  (if (nth-value 1 (gethash design dp))
      (gethash design dp)
      (let ((ret
              (if (null design)
                  1
                  (iter
                    (for towel in towels)
                    (for (values matched rest-design) =
                         (match-design design towel))
                    (when matched
                      (sum (match-full-design rest-design towels dp)))))))
        (setf (gethash design dp) ret)
        ret)))

(defun day19 (input &key (part 1))
  (destructuring-bind (towels designs) (run-parser (parse-all) input)
    (setf towels (sort towels #'> :key #'length))
    (iter
      (with dp = (make-hash-table :test 'equal))
      (for design in designs)
      (if (= part 1)
          (counting (not (= 0 (match-full-design design towels dp))))
          (sum (match-full-design design towels dp))))))
