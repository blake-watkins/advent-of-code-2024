(in-package :aoc-2024)

(defun parse-grid ()
  (parse-lines (one-or-more (parse-character "XMAS"))))

(defun test-indices (grid indices)
  (or (every (lambda (char idx) (char= char (gethash idx grid))) "XMAS" indices)
      (every (lambda (char idx) (char= char (gethash idx grid))) "SAMX" indices)))

(defun count-horizontal (grid r size)
  (iter
    (for c to (- size 4))
    (count (test-indices grid (iter (for i below 4) (collect (list r (+ c i))))))))

(defun count-vertical (grid c size)
  (iter
    (for r to (- size 4))
    (count (test-indices grid (iter (for i below 4) (collect (list (+ r i) c)))))))

(defun count-negative-diagonal (grid size)
  (iter
    (for r to (- size 4))
    (sum (iter
           (for c to (- size 4))
           (count (test-indices grid (iter (for i below 4) (collect (list (+ r i) (+ c i))))))))))

(defun count-positive-diagonal (grid size)
  (iter
    (for r from 3 below size)
    (sum (iter
           (for c to (- size 4))
           (count (test-indices grid (iter (for i below 4) (collect (list (- r i) (+ c i))))))))))

(defparameter *mas* '((#\M #\. #\M)
                      (#\. #\A #\.)
                      (#\S #\. #\S)))

(defun match-mas (grid r c)
  (iter
    (for i below (length *mas*))
    (always
     (iter
       (for j below (length (first *mas*)))
       (for mas-char = (elt (elt *mas* i) j))
       (for grid-char = (gethash (list (+ r i) (+ c j)) grid))
       (always (or (char= mas-char #\.) (char= mas-char grid-char)))))))

(defun part2 (grid size)
  (iter
    (for r to (- size (length *mas*)))
    (appending (iter
                 (for c to (- size (length (first *mas*))))
                 (when (match-mas grid r c) (collect (list r c)))))))

(defun rotate-grid (grid size)
  (let ((ret (make-hash-table :test 'equal)))
    (iter
      (for r below size)
      (iter
        (for c below size)
        (setf (gethash (list r c) ret) (gethash (list c (- size r 1)) grid)))
      (finally (return ret)))))

(defun day4 (input &key (part 1))
  (let* ((parsed (run-parser (parse-grid) input))
         (grid (hash-table-from-list-list parsed))
         (size (length parsed)))
    (if (= part 1)
        (+ (iter
             (for r from 0 below size)
             (sum (count-horizontal grid r size)))
           (iter
             (for c from 0 below size)
             (sum (count-vertical grid c size)))
           (count-positive-diagonal grid size)
           (count-negative-diagonal grid size))
        (iter
          (for g initially grid then (rotate-grid g size))
          (repeat 4)
          (sum (length (part2 g size)))))))
