(defpackage #:day17
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:map-product))

(in-package #:day17)

(named-readtables:in-readtable :aoc)

(defparameter *example*
  ".#.
..#
###")

(defun repeat (count item)
  (loop repeat count collect item))

(defun solve (input &optional (dimensions 3))
  (let ((grid (make-hash-table :test 'equal))
        (lines (str:lines input)))
    (loop for y below (length lines) do
      (loop for x below (length (elt lines y)) do
        (when (eq #\# (elt (elt lines y) x))
          (setf (gethash `(,x ,y ,@(repeat (- dimensions 2) 0)) grid) t))))
    (loop repeat 6 do (setf grid (evolve grid)))
    (ht-count grid)))

(defun should-stay? (grid pos)
  (let ((alive? (gethash pos grid))
        (count (count-if #`(gethash % grid) (neighbours pos))))
    (or (and alive? (<= 2 count 3))
        (and (not alive?) (eq count 3)))))

(defun evolve (grid)
  (let ((affected-cells (make-hash-table :test 'equal))
        (next (make-hash-table :test 'equal)))

    (dotable (pos _ grid)
      (mapcar #`(setf (gethash % affected-cells) t)
              (neighbours pos)))

    (dotable (pos _ affected-cells)
      (when (should-stay? grid pos)
        (setf (gethash pos next) t)))

    next))

(defun neighbours (pos)
  "Returns all neighbours of any-dimensional POS defined as list of
integer coordinates."
  (when pos
    (let (result)
      (apply #'map-product (lambda (&rest delta)
                             (unless (apply #'= 0 delta)
                               (push (mapcar #'+ pos delta) result)))
             (repeat (length pos) '(-1 0 1)))
      result)))

(solve (aoc:puzzle 17 2020)) ;; 291
(solve (aoc:puzzle 17 2020) 4) ;; 1524
