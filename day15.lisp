(in-package :aoc-2024)

(defun parse-map ()
  (bind (parse-lines (one-or-more (parse-character "#.O@")))
        (lambda (ll) (unit (hash-table-from-list-list ll)))))

(defun parse-map-2 ()
  (bind (parse-lines
         (bind (one-or-more
                (bind (parse-character "#.O@")
                      (lambda (c)
                        (unit (case c
                                (#\# '(#\# #\#))
                                (#\. '(#\. #\.))
                                (#\O '(#\[ #\]))
                                (#\@ '(#\@ #\.)))))))
               (lambda (ll) (unit (apply #'concatenate 'list ll)))))
        (lambda (ll) (unit (hash-table-from-list-list ll)))))

(defun parse-instructions ()
  (with-monad
    (assign lines (parse-lines (one-or-more (bind (parse-character "<>^v")
                                                  (lambda (c)
                                                    (unit (case c
                                                            (#\< :left)
                                                            (#\> :right)
                                                            (#\^ :up)
                                                            (#\v :down))))))))
    (unit (apply #'concatenate 'list lines))))

(defun parse-all ()
  (with-monad
    (assign map (parse-map))
    (parse-newline)
    (parse-newline)
    (assign instrs (parse-instructions))
    (unit (list map instrs))))

(defun parse-all-2 ()
  (with-monad
    (assign map (parse-map-2))
    (parse-newline)
    (parse-newline)
    (assign instrs (parse-instructions))
    (unit (list map instrs))))


(defun move (pos dir)
  (point+ pos (case dir
                (:up '(-1 0))
                (:down '(1 0))
                (:left '(0 -1))
                (:right '(0 1)))))

(defun can-move (pos dir map)
  (iter
    (for cur first pos then (move cur dir))
    (for cur-square = (gethash cur map))
    (until (or (char= #\. cur-square) (char= #\# cur-square)))
    (finally (return (when (char= #\. cur-square) cur)))))


(defun move-box-left (pos map)
  (let ((new-map (if (is-free pos :left map)
                     map
                     (move-item (move pos :left) :left map))))
    (when new-map
      (rotatef (gethash (move pos :left) new-map)
               (gethash pos new-map)
               (gethash (move pos :right) new-map))
      new-map)))

(defun move-box-right (pos map)
  (let ((new-map (if (is-free (move pos :right) :right map)
                     map
                     (move-item (move (move pos :right) :right) :right map))))
    (when new-map
      (rotatef (gethash (move (move pos :right) :right) new-map)
               (gethash (move pos :right) new-map)
               (gethash pos new-map))
      new-map)))

(defun move-box-up (pos map)
  (let ((new-map (and (if (is-free pos :up map)
                          map
                          (move-item (move pos :up) :up map))
                      (if (is-free (move pos :right) :up map)
                          map
                          (move-item (move (move pos :right) :up) :up map)))))
    (when new-map
      (rotatef (gethash pos map) (gethash (move pos :up) map))
      (rotatef (gethash (move pos :right) map) (gethash (move (move pos :right) :up) map))
      new-map)))

(defun move-box-down (pos map)
  (let ((new-map (and (if (is-free pos :down map)
                          map
                          (move-item (move pos :down) :down map))
                      (if (is-free (move pos :right) :down map)
                          map
                          (move-item (move (move pos :right) :down) :down map)))))
    (when new-map
      (rotatef (gethash pos map) (gethash (move pos :down) map))
      (rotatef (gethash (move pos :right) map) (gethash (move (move pos :right) :down) map))
      new-map)))

(defun move-box (pos dir map)
  (case dir
    (:left (move-box-left pos map))
    (:right (move-box-right pos map))
    (:up (move-box-up pos map))
    (:down (move-box-down pos map))))

(defun move-item (pos dir map)
  (let ((cur-square (gethash pos map)))
    (case cur-square
      (#\# nil)
      (#\@
       (let ((new-map (if (is-free pos dir map)
                          map
                          (move-item (move pos dir) dir map))))
         (when new-map
           
           (rotatef (gethash (move pos dir) new-map) (gethash pos new-map))
           new-map)))
      (#\[ (move-box pos dir map))
      (#\] (move-box (move pos :left) dir map)))))


(defun print-map (map)
  (format t "~{~{~a~}~%~}"
          (iter
            (with (rows cols) = (hash-table-dimensions map))
            (for r to rows)
            (collect
                (iter
                  (for c to cols)
                  (collect (gethash (list r c) map)))))))

(defun find-start (map)
  (iter
    (for (pos val) in-hashtable map )
    (finding pos such-that (char= #\@ val))))

(defun day15 (input)
  (destructuring-bind (map instructions) (run-parser (parse-all-2) input)
     (iter
       (with cur = (find-start map))
       (for i from 1)
       (for instr in instructions)
       (for new-map = (move-item cur instr (alexandria:copy-hash-table map)))
       (when new-map
         (setf map new-map)
         (setf cur (move cur instr)))
       (finally (print-map map) (return (gps-coords-2 map))))))

(defun gps-coords-2 (map)
  (iter
    (for ((r c) char) in-hashtable map)
    (when (char= #\[ char)
      (sum (+ (* 100 r) c)))))

(defun gps-coords (map)
  (iter
    (for ((r c) char) in-hashtable map)
    (when (char= #\O char)
      (sum (+ (* 100 r) c)))))
