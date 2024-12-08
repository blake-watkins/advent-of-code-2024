(in-package :aoc-2024)

(defun parse-map ()
  (bind (parse-lines (one-or-more (parse-character ".#^")))
        (lambda (chars)
          (let ((start-pos
                  (iter
                    (for r index-of-sequence chars)
                    (for row in chars)
                    (thereis
                     (iter
                       (for c index-of-sequence row)
                       (for square in row)
                       (finding (list r c) such-that (char= #\^ square)))))))
            (unit (list start-pos (hash-table-from-list-list chars)))))))

(defparameter *dirs* '((:up . (-1 0))
                       (:right . (0 1))
                       (:down . (1 0))
                       (:left . (0 -1))))

(defun move (cur dir)
  (mapcar #'+ cur (cdr (assoc dir *dirs*))))

(defun turn-right (dir)
  (first (elt *dirs* (mod (1+ (position dir *dirs* :key #'first))
                          (length *dirs*)))))

(defun patrol-route (start map)
  (iter
    (with visited = (make-hash-table :test 'equal))
    (with cur = start)
    (with dir = :up)
    (until (gethash (list dir cur) visited))
    (setf (gethash (list dir cur) visited) t)
    (for next = (move cur dir))
    (for next-square = (gethash next map))
    (while next-square)
    (if (char= #\# next-square)
        (setf dir (turn-right dir))
        (setf cur next))
    (finally (return (values next-square visited)))))

(defun visited-squares (visited)
  (iter
    (with visited-squares = (make-hash-table :test 'equal))
    (for ((nil pos) v) in-hashtable visited)
    (setf (gethash pos visited-squares) t)
    (finally (return visited-squares))))

(defun day6 (input &key (part 1))
  (destructuring-bind (start map) (run-parser (parse-map) input)
    (multiple-value-bind (next visited) (patrol-route start map)
      (declare (ignore next))
      (let ((visited-squares (visited-squares visited)))
       (if (= part 1)
           (hash-table-count visited-squares)
           (iter
             (for (pos . nil) in-hashtable visited-squares)
             (unless (equal pos start)
               (setf (gethash pos map) #\#)
               (counting (patrol-route start map))
               (setf (gethash pos map) #\.))))))))
