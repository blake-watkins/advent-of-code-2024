(in-package :aoc-2024)

(defun parse-map ()
  (bind (parse-lines (one-or-more (parse-character ".#SE")))
        (lambda (ll) (unit (convert-map ll)))))

(defun convert-map (ll)
  (let* ((ret (hash-table-from-list-list ll)))
    (destructuring-bind (start-pos end-pos)
        (iter
          (for (k v) in-hashtable ret)
          (when (char= v #\S) (collect k into start-pos))
          (when (char= v #\E) (collect k into end-pos))
          (finally (return (list start-pos end-pos))))
      (setf (gethash start-pos ret) #\.)
      (setf (gethash end-pos ret) #\.)
      (list ret (first start-pos) (first end-pos)))))

(defparameter *dirs* '((-1 0) (1 0) (0 -1) (0 1)))
(defparameter *cheats* '((-2 0) (-1 -1) (0 -2) (1 -1)
                         (2 0) (1 1) (0 2) (-1 1)))

(defun neighbours (pos map)
  (iter
    (for dir in *dirs*)
    (for test = (point+ pos dir))
    (when (char= (gethash test map) #\.) (collect test))))

(defun shortest-path (cur end visited map)
  (if (equal cur end)
      end
      (let ((neighbours (neighbours cur map)))
        (setf (gethash cur visited) t)
        (iter
          (for neighbour in neighbours)
          (unless (gethash neighbour visited)
            (for path = (shortest-path neighbour end visited map))
            (finding path minimizing (length path) into n-path))
          (finally (return (when n-path (cons cur n-path))))))))
