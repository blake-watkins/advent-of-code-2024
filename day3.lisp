(in-package :aoc-2024)

(defun parse-mul-num ()
  (with-monad
    (assign first (parse-digit))
    (assign rest (up-to-n-of 2 (parse-digit)))
    (unit (digits-to-int (cons first rest) :base 10))))

(defun parse-mul ()
  (with-monad
    (parse-string "mul(")
    (assign a (parse-mul-num))
    (parse-string ",")
    (assign b (parse-mul-num))
    (parse-string ")")
    (unit (* a b))))

(defun parse-command ()
  (either
   (parse-mul)
   (then (parse-string "don't()") (unit :dont))
   (then (parse-string "do()") (unit :do))))

(defun day3 (input &key (part 1))
  (let ((parsed (run-parser (zero-or-more (parse-until (parse-command))) input)))
    (iter
      (with enabled = t)
      (for x in parsed)
      (cond
        ((and (numberp x)
              (or (= part 1) enabled))
         (sum x))
        ((eq :dont x) (setf enabled nil))
        ((eq :do x) (setf enabled t))))))
