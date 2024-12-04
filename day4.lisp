(in-package :aoc-2024)

(defun parse-grid ()
  (parse-lines (one-or-more (parse-character "XMAS"))))

(defun rotate-grid (grid size)
  (let ((ret (make-hash-table :test 'equal)))
    (iter
      (for r below size)
      (iter
        (for c below size)
        (setf (gethash (list r c) ret) (gethash (list c (- size r 1)) grid)))
      (finally (return ret)))))

(defun test-indices (grid indices)
  (every (lambda (char idx) (char= char (gethash idx grid))) "XMAS" indices))

(defun count-horizontal (grid size)
  (iter
    (for r to (- size 1))
    (sum (iter
           (for c to (- size 4))
           (for indices =
                (iter (for i below 4) (collect (list r (+ c i)))))
           (count (test-indices grid indices))))))

(defun count-diagonal (grid size)
  (iter
    (for r to (- size 4))
    (sum (iter
           (for c to (- size 4))
           (for indices =
                (iter (for i below 4) (collect (list (+ r i) (+ c i)))))
           (count (test-indices grid indices))))))

(defun count-mas (grid size)
  (let* ((mas '((#\M #\. #\M)
                (#\. #\A #\.)
                (#\S #\. #\S)))
         (mas-size (length mas)))
    (labels ((test-mas (r c)
               (iter
                 (for i below mas-size)
                 (always (iter
                           (for j below mas-size)
                           (for mas-char = (elt (elt mas i) j))
                           (for grid-char = (gethash (list (+ r i) (+ c j)) grid))
                           (always (or (char= mas-char #\.)
                                       (char= mas-char grid-char))))))))
      (iter
        (for r to (- size mas-size))
        (sum (iter
               (for c to (- size mas-size))
               (count (test-mas r c))))))))

(defun day4 (input &key (part 1))
  (let* ((parsed (run-parser (parse-grid) input))
         (grid (hash-table-from-list-list parsed))
         (size (length parsed)))
    (iter
      (for g initially grid then (rotate-grid g size))
      (repeat 4)
      (if (= part 1)
          (progn
            (sum (count-horizontal g size))
            (sum (count-diagonal g size)))
          (sum (count-mas g size))))))
