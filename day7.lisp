(in-package :aoc-2024)

(defun parse-equations ()
  (parse-lines
   (with-monad
     (assign target (parse-number))
     (parse-string ": ")
     (assign numbers (parse-list (parse-number) #\Space))
     (unit (list target numbers)))))

(defun concat (a b)
  (parse-integer (format nil "~a~a" a b)))

(defun test-target (target cur numbers)
  (if numbers
      (or (test-target target (+ cur (first numbers)) (rest numbers))
          (test-target target (* cur (first numbers)) (rest numbers))
          (test-target target (concat cur (first numbers))  (rest numbers)))
      (= target cur)))

(defun day7 (input)
  (iter
    (for (target numbers) in (run-parser (parse-equations) input))
    (when (test-target target 0 numbers)
      (sum target))))
