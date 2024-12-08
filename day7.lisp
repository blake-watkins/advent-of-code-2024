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

(defun test-target (target cur numbers ops)
  (if numbers
      (iter
        (for op in ops)
        (thereis
         (test-target target (funcall op cur (first numbers)) (rest numbers) ops)))
      (= target cur)))

(defun day7 (input &key (part 1))  
  (iter
    (with ops = (if (= part 1) (list #'* #'+) (list #'* #'+ #'concat)))
    (for (target numbers) in (run-parser (parse-equations) input))
    (when (test-target target 0 numbers ops)
      (sum target))))
