(defpackage #:day01-2016 (:use #:cl))

(in-package #:day01-2016)

(defun part1 (input)
  (let ((dx 0) (dy 1) (x 0) (y 0))
    (loop for rule in (str:split ", " input) do
      (case (elt rule 0)
        (#\L (psetf dy dx dx (- dy)))
        (#\R (psetf dy (- dx) dx dy)))
      (let ((steps (parse-integer (subseq rule 1) :junk-allowed t)))
        (incf x (* steps dx))
        (incf y (* steps dy))))
    (+ (abs x) (abs y))))

(part1 "R5, L5, R5, R3") ;; 12

(part1 (aoc:puzzle 1 2016)) ;; 161

(defun part2 (input)
  (let ((visited (make-hash-table :test 'equal))
        (dx 0) (dy 1) (x 0) (y 0))
    (loop for rule in (str:split ", " input) do
      (case (elt rule 0)
        (#\L (psetf dy dx dx (- dy)))
        (#\R (psetf dy (- dx) dx dy)))
      (loop repeat (parse-integer (subseq rule 1) :junk-allowed t) do
        (incf x dx)
        (incf y dy)
        (when (gethash (cons x y) visited)
          (return-from part2 (+ (abs x) (abs y))))
        (setf (gethash (cons x y) visited) t)))))

(part2 "R8, R4, R4, R8") ;; 4

(part2 (aoc:puzzle 1 2016)) ;; 110
