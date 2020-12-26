(defpackage #:day03
  (:use #:cl))

(in-package #:day03)

(defparameter *map*
  (with-open-file (in "2020/day03.txt")
    (let ((map (loop for line = (read-line in nil) while line collect line)))
      (make-array (length map) :initial-contents map))))

(defun traverse (right down)
  (let ((m (length (elt *map* 0))))
    (loop for x = 0 then (mod (+ right x) m)
          for y = 0 then (+ y down)
          while (< y (length *map*))
          count (eq #\# (elt (elt *map* y) x)))))

(defun solve ()
  (values
   (traverse 3 1)
   (* (traverse 1 1)
      (traverse 3 1)
      (traverse 5 1)
      (traverse 7 1)
      (traverse 1 2))))

(solve)
