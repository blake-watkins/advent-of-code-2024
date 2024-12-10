(in-package :aoc-2024)

(defun parse-filesystem () (zero-or-more (parse-digit)))

(defun build-filesystem (blocks)
  (iter
    (with index = 0)
    (for type first :block then (if (eq type :block) :free :block))
    (for size in blocks)
    (generating id from 0)
    (if (eq type :block)
        (collect (list index size (next id)) into block-list)
        (collect (list index size) into free-list))
    (incf index size)
    (finally (return (list (reverse block-list) free-list)))))

(defun move-blocks (blocks free)
  (if (and free blocks (> (caar blocks) (caar free)))
      (destructuring-bind (block-idx block-size block-id) (first blocks)
        (destructuring-bind (free-idx free-size) (first free)
          (let ((move-amt (min block-size free-size (- block-idx free-idx))))
            (cons
             (list free-idx move-amt block-id)
             (move-blocks
              (if (= block-size move-amt)
                  (rest blocks)
                  (cons (list block-idx (- block-size move-amt) block-id)
                        (rest blocks)))
              (if (= free-size move-amt)
                  (rest free)
                  (cons (list (+ free-idx move-amt) (- free-size move-amt))
                        (rest free))))))))
      blocks))

(defun find-free-space (size free-list)
  "Find first free block of size SIZE. Return NIL or the index as first return value, and the new free list as the second return value. "
  (if (null free-list)
      (values nil nil)
      (destructuring-bind (index free-size) (first free-list)
        (if (<= size free-size)
            (if (= free-size size)
                (values index (rest free-list))
                (values index (cons (list (+ index size) (- free-size size))
                                    (rest free-list))))
            (multiple-value-bind (ret-idx ret-list)
                (find-free-space size (rest free-list))
              (values ret-idx (cons (first free-list) ret-list)))))))

(defun move-blocks-2 (block-list free-list)
  (iter
    (for (idx size id) in block-list)
    (for (values new-idx new-free-list) = (find-free-space size free-list))
    (if (and new-idx (< new-idx idx))
        (progn
          (collect (list new-idx size id))
          (setf free-list new-free-list))
        (collect (list idx size id)))))

(defun checksum (blocks)
  (iter
    (for (idx size id) in blocks)
    (sum (iter
           (for i from idx)
           (repeat size)
           (sum (* i id))))))

(defun day9 (input &key (part 1))
  (let ((blocks-free (build-filesystem (run-parser (parse-filesystem) input))))
    (checksum (apply (if (= part 1) #'move-blocks #'move-blocks-2) blocks-free))))
