(in-package :aoc-2024)

(defun parse-map ()
  (bind (parse-lines (one-or-more (parse-alphanumeric)))
        (lambda (ll) (unit (hash-table-from-list-list ll)))))

(defparameter *dirs* '((-1 0) (1 0) (0 -1) (0 1)))

(defun area-perimeter (type cur seen map)
  (setf (gethash cur seen) t)
  (let ((child-values
          (iter
            (for dir in *dirs*)
            (for next = (point+ cur dir))
            (when (and (gethash next map)
                       (not (gethash next seen))
                       (char= type (gethash next map)))
              (collect (area-perimeter type next seen map)))))
        (perimeter
          (iter
            (for dir in *dirs*)
            (for next = (point+ cur dir))
            (counting
             (not (and (gethash next map) (char= type (gethash next map))))))))
    (reduce #'point+ (cons (list 1 perimeter) child-values))))

(defun get-regions (cur uf seen map)
  (setf (gethash cur seen) t)
  (uf-make-set cur uf)
  (iter
    (for dir in *dirs*)
    (for next = (point+ cur dir))
    (for next-square = (gethash next map))
    (when next-square
      (uf-make-set next uf)
      (when (char= next-square (gethash cur map))
        (uf-union cur next uf))
      (when (not (gethash next seen))
        (get-regions next uf seen map))))
  uf)

(defun day12 (input)
  (let ((map (run-parser (parse-map) input))
        (seen (make-hash-table :test 'equal)))
    (iter
      (for (pos type) in-hashtable map)
      (when (not (gethash pos seen))
        (sum (apply #'* (area-perimeter type pos seen map)))))))

(defun get-region-perimeters (regions map)
  (declare (optimize (debug 3)))
  (iter
    (with ret = (make-hash-table :test 'equal))    
    (for cur in (alexandria:hash-table-keys map))
    (For cur-square = (gethash cur map))
    (iter
      (for dir in *dirs*)
      (for next = (point+ dir cur))
      (for next-square = (gethash next map))
      (when (or (not next-square) (not (char= cur-square next-square)))
        (push (list cur dir) (gethash (uf-find cur regions) ret '()))))
    (finally (return ret))))

()
(defun collapse-perimeters (edges)
  (let ((collapsed (make-uf :test 'equal)))
    (iter
      (for edge in edges)
      (uf-make-set edge collapsed))
    (iter
      (for (pos dir) in edges)
      (iter
        (for next in (alexandria:switch (dir :test 'equal)
                       ('(1 0) '((0 -1) (0 1)))
                       ('(-1 0) '((0 -1) (0 1)))
                       ('(0 1) '((1 0) (-1 0)))
                       ('(0 -1) '((1 0) (-1 0)))))
        (uf-union (list pos dir) (list (point+ pos next) dir) collapsed))
      (finally (return collapsed)))))

(defun day12-2 (input)
  (let ((map (run-parser (parse-map) input))
        (seen (make-hash-table :test 'equal))
        (regions (make-uf :test 'equal)))
    (get-regions '(0 0) regions seen map)
    (let ((perimeters (get-region-perimeters regions map)))
      (iter
        (for (region perimeter) in-hashtable perimeters)
        (for num-sides = (length (uf-roots (collapse-perimeters perimeter))))
        (sum (* (uf-set-size region regions) num-sides))))))
