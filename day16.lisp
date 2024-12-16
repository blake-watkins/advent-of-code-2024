(in-package :aoc-2024)

(defun parse-map ()
  (with-monad
    (assign ll (parse-lines (one-or-more (parse-character "#.SE"))))
    (unit (hash-table-from-list-list ll))))

(defun find-in-map (char map)
  (iter
    (for (k v) in-hashtable map)
    (finding k such-that (char= char v))))

(defparameter *dirs* '((:east (0 1))
                       (:south (1 0))
                       (:west (0 -1))
                       (:north (-1 0))))

(defun neighbours (vertex map)
  (destructuring-bind (pos dir) vertex
    (let* ((offset (second (find dir *dirs* :key #'first)))
           (straight `((,(point+ pos offset) ,dir) 1))
           (idx (position dir *dirs* :key #'first))
           (turns (mapcar (lambda (i)
                            `((,pos ,(first (elt *dirs* (mod i 4)))) 1000))
                          (list (1+ idx) (1- idx)))))
      (remove-if (lambda (v) (char= #\# (gethash (caar v) map)))
                 (cons straight turns)))))

(defun day16 (input)
  (let* ((map (run-parser (parse-map) input))
         (start-pos (find-in-map #\S map))
         (end-pos (find-in-map #\E map)))
    (iter
      (for vertex in-dijkstra-from (list start-pos :east)
           neighbours (lambda (n) (neighbours n map)))
      (until (equal (caar vertex) end-pos))
      (finally (return vertex)))))
