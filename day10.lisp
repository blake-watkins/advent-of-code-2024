(in-package :aoc-2024)

(defun parse-map ()
  (parse-lines (one-or-more (parse-digit))))

(defun moves (pos map)
  (iter
    (for d in '((-1 0) (1 0) (0 -1) (0 1)))
    (for test = (point+ d pos))
    (when (gethash test map)
      (collect test))))

(defun score (cur visited map)
  (when visited (setf (gethash cur visited) t))
  (let ((square (gethash cur map)))
    (if (= square 9)
        1
        (iter
          (for next in (moves cur map))
          (for next-square = (gethash next map))
          (when (and (or (null visited) (not (gethash next visited)))
                     (= next-square (1+ square)))
            (sum (score next visited map)))))))

(defun day10 (input &key (part 1))
  (let* ((parsed (run-parser (parse-map) input))
         (map (hash-table-from-list-list parsed)))
    (iter
      (for start in (alexandria:hash-table-keys map))      
      (when (= 0 (gethash start map))
        (let ((visited (if (= part 1)
                           (make-hash-table :test 'equal)
                           '())))
          (sum (score start visited map)))))))
