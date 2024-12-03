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
    (unit (list a b))))

(defun parse-commands (enabled)
  (either
   (with-monad
     (if enabled
         (either
          (with-monad
            (assign ret (parse-mul))
            (assign rest (parse-commands enabled))
            (unit (cons ret rest)))
          (then (parse-string "don't()") (parse-commands nil)))
         (then (parse-string "do()") (parse-commands t))))
   (with-monad
     (parse-any-character)
     (parse-commands enabled))
   (unit '())))

(defun day3 (input &key (part 1))
  (let ((parsed (run-parser
                 (if (= part 1)
                     (zero-or-more (parse-until (parse-mul)))
                     (parse-commands t))
                 input)))
    (iter (for (a b) in parsed) (sum (* a b)))))
