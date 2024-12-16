(in-package :aoc-2024)

(defun parse-robots ()
  (parse-lines (with-monad
                 (parse-string "p=")
                 (assign pos (parse-number-list))
                 (parse-string " v=")
                 (assign vel (parse-number-list))
                 (unit (mapcar #'reverse (list pos vel))))))
(defparameter *dim* '(7 11))

(defun update-robot (pos vel steps dim)
  (mapcar (lambda (p v d) (mod (+ p (* v steps)) d)) pos vel dim))

(defun quadrant (pos dim)
  (let ((indicate (mapcar (lambda (p d) (signum (- p (floor d 2))))
                          pos dim)))
    (unless (some #'zerop indicate)
      (alexandria:switch (indicate :test 'equal)
        ('(1 1) 4)
        ('(1 -1) 3)
        ('(-1 1) 2)
        ('(-1 -1) 1)))))

(defun print-robots (robots dim)
  (let ((robots-ht (make-hash-table :test 'equal)))
    (iter
      (for (pos nil) in robots)
      (incf (gethash pos robots-ht 0)))
    (format t "~{~{~a~}~%~}"
            (iter
              (for r below (first dim))              
              (collect
                  (iter
                    (for c below (second dim))
                    (for square = (gethash (list r c) robots-ht 0))
                    (collect (cond 
                               ((= 0 square) #\.)
                               ((< square 10) (code-char (+ (char-code #\0) square)))
                               (t #\*)))))))))

(defun nshuffle (seq)
  (iter
    (for i from (length seq) downto 2)
    (rotatef (elt seq (random i)) (elt seq (1- i))))
  seq)

(defun get-point (t1 t2 p1 p2 dim)
  (let* ((dt (- t2 t1))
         (dt-1 (mapcar (lambda (x) (invmod dt x)) dim)))
    (mapcar (lambda (p1 p2)
                      (let* ((dp (point- p2 p1))
                             (v (mapcar (lambda (d dt-1 m) (mod (* dt-1 d) m)) dp dt-1 dim))
                             (p0 (mapcar (lambda (p v m) (mod (+ p (* v (- t1))) m)) p1 v dim)))
                        (list  p0 v)))
                    p1
                    p2)))

(defun quadrants (robots dim)
  (iter
    (with quadrants = (make-hash-table :test 'eql))
    (for (pos nil) in robots)
    (for quadrant = (quadrant pos dim))
    (when quadrant
      (incf (gethash quadrant quadrants 0)))
    (finally (return quadrants))))

(defun quadrant-diff (quadrants)
  (let ((values (alexandria:hash-table-values quadrants)))
    (- (reduce #'max values) (reduce #'min values))))

(defun day14 (input dim)
  (iter
    (with quadrant-diff = 0)
    (for steps from 0)
    (for robots first (run-parser (parse-robots) input) then
         (mapcar (lambda (robot)
                   (destructuring-bind (pos vel) robot
                     (list (update-robot pos vel 1 dim)
                           vel)))
                 robots))
    (for diff = (quadrant-diff (quadrants robots dim)))
    (when (> diff quadrant-diff)
      (format t "~a~%" steps)
      (print-robots robots dim)
      (format t "~%")
      (setf quadrant-diff diff))))
