(in-package :aoc-2024)

(defun parse-behaviour ()
  (with-monad
    (assign xy (parse-keyword #'alpha-char-p))
    (parse-string "+")
    (assign amt (parse-number))
    (unit amt)))

(defun parse-button ()
  (with-monad
    (parse-string "Button ")
    (assign ab (parse-keyword #'alpha-char-p))
    (parse-string ": ")
    (assign behaviour (parse-list (parse-behaviour)
                                  (parse-string ", ")))
    (unit behaviour)))

(defun parse-prize ()
  (parse-list (with-monad
                (assign xy (parse-keyword #'alpha-char-p))
                (parse-string "=")
                (assign amt (parse-number))
                (unit amt))
              ", "))

(defun parse-machine ()
  (with-monad
    (assign buttons (parse-lines (parse-button)))
    (parse-newline)
    (parse-string "Prize: ")
    (assign prize (parse-prize))
    (parse-newline)
    (unit (list buttons prize))))

(defun mat-inv (mat)
  (destructuring-bind (a b c d) mat
    (let ((det (- (* a d) (* b c))))
      (unless (= 0 det)
        (mapcar (lambda (x) (/ x det)) (list d (- b) (- c) a))))))

(defun mat* (mat vec)
  (destructuring-bind (a b c d) mat
    (destructuring-bind (x y) vec
      (list (+ (* a x) (* b y))
            (+ (* c x) (* d y))))))

(defun day13 (input)
  (let ((machines (run-parser (parse-lines (parse-machine)) input)))
    (iter
      (for (((a b) (c d)) (x y)) in machines)
      (for inv = (mat-inv (list a c b d)))
      (when inv
        (for presses = (mat* inv (mapcar (lambda (v) (+ v 10000000000000)) (list x y))))
        (when (every #'integerp presses)
          (sum
           (+ (* 3 (first presses)) (second presses))))))))
