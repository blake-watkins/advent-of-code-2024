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

(defun dijkstra (vertex neighbours-fn)
  (let ((visited (fset:empty-set))
	(distance-to (fset:empty-map))
        (parents (fset:empty-map (fset:empty-set)))
	(frontier (make-instance 'cl-heap:priority-queue)))
    (fset:includef distance-to vertex 0)
    (cl-heap:enqueue frontier (list vertex nil) 0)
    
    (iter
      (until (zerop (cl-heap:queue-size frontier)))
      (for (vertex nil) = (cl-heap:dequeue frontier))
      (for distance = (fset:lookup distance-to vertex))
      (unless (fset:lookup visited vertex)
        (fset:includef visited vertex)
        (iter
          (for (neighbour neighbour-distance) in (funcall neighbours-fn vertex))
          (let ((tentative-distance (+ distance neighbour-distance)))
            (when (or (not (fset:lookup visited neighbour))
                      (<= tentative-distance
                          (fset:lookup distance-to neighbour)))
              (cl-heap:enqueue frontier
			       (list neighbour vertex)
			       tentative-distance)	        

              (if (or (not (fset:lookup distance-to neighbour))
                      (< tentative-distance (fset:lookup distance-to neighbour)))
                  (setf (fset:lookup parents neighbour)
                        (fset:with (fset:empty-set) (list vertex tentative-distance)))
                  (fset:includef (fset:lookup parents neighbour) (list vertex tentative-distance)) 
                  )
              
              (fset:includef distance-to neighbour tentative-distance)))))

      (finally (return parents)))))

(defun day16 (input)
  (let* ((map (run-parser (parse-map) input))
         (start-pos (find-in-map #\S map))
         (end-pos (find-in-map #\E map)))
    (iter
      (for vertex in-dijkstra-from (list start-pos :east)
           neighbours (lambda (n) (neighbours n map)))
      (until (equal (caar vertex) end-pos))
      (finally (return vertex)))))

(defun day16-2 (input)
  (let* ((map (run-parser (parse-map) input))
         (start-pos (find-in-map #\S map))
         (end-pos (find-in-map #\E map))
         (parents 
           (dijkstra `(,start-pos :east) (lambda (n) (neighbours n map))))
         (last-vertex
           (iter
	     (for vertex in-fset parents)
	     (when (equal (first vertex) end-pos)
	       (finding vertex minimizing 
		        (fset:reduce #'min
                                     (fset:lookup parents vertex)
                                     :key #'second))))))
    (iter
      (with visited = (fset:empty-set))
      (with to-visit = (fset:with (fset:empty-set) last-vertex))
      (until (fset:empty? to-visit))
      (for vertex = (fset:arb to-visit))
      (fset:removef to-visit vertex)
      (fset:includef visited (first vertex))
      (setf to-visit
            (fset:union to-visit
                        (fset:image #'first (fset:lookup parents vertex))))
      (finally (return (fset:size visited))))))
