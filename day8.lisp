(in-package :aoc-2024)

(defun parse-map ()
  (parse-lines (one-or-more (either (parse-character #\.) (parse-alphanumeric)))))

(defun get-dimensions (map)
  (list (length map) (length (first map))))

(defun get-freqs (map)
  (iter
    (with ret = (make-hash-table))
    (for r from 0)
    (for row in map)
    (iter
      (for c from 0)
      (for square in row)
      (unless (char= #\. square)
        (push (list r c) (gethash square ret '()))))
    (finally (return ret))))

(defun get-antinodes (a b dimensions part)
  (labels ((in-bounds (point)
             (every (lambda (p d) (<= 0 p (1- d))) point dimensions))
           (antinodes-from-indexes (idxs)
             (remove-if-not
              #'in-bounds
              (mapcar (lambda (i) (point+ a (point* i (point- b a)))) idxs))))
    (if (= part 1)
        (antinodes-from-indexes '(-1 2))
        (iter
          (for i from 0)
          (for antinodes = (antinodes-from-indexes (list i (- i))))
          (while antinodes)
          (appending antinodes)))))

(defun day8 (input &key (part 1))
  (let* ((map (run-parser (parse-map) input))
         (freqs (get-freqs map))
         (dimensions (get-dimensions map))
         (antinodes (make-hash-table :test 'equal)))
    (iter
      (for (freq antennas) in-hashtable freqs)
      (iter
        (for (a b) in (pairs antennas))
        (iter
          (for antinode in (get-antinodes a b dimensions part))
          (setf (gethash antinode antinodes) t)))
      (finally (return (hash-table-count antinodes))))))
