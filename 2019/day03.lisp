(in-package :advent-of-code-2019-day3)

(defun read-input-from-string (string)
  (mapcar (lambda (line)
            (mapcar (lambda (vec) (cons (elt vec 0) (parse-integer (subseq vec 1)))) (str:split #\, line)))
          (str:lines string)))

(defparameter *input*
  (read-input-from-string (rutils:slurp "day03.txt")))

(defparameter *points1* (build-stops (elt *input* 0)))
(defparameter *points2* (build-stops (elt *input* 1)))

(defconstant +start+ (cons 0 0))

(defun delta (direction)
  (ecase direction
    (#\R '(1 0))
    (#\L '(-1 0))
    (#\U '(0 1))
    (#\D '(0 -1))))

(defun build-stops (vectors)
  (loop
    :with points := (list (cons 0 0))
    :with x := 0 :and y := 0
    :for (dir . dist) :in vectors
    :do
       (destructuring-bind (dx dy) (delta dir)
         (incf x (* dx dist))
         (incf y (* dy dist))
         (push (cons x y) points))
    :finally
       (return (nreverse points))))

;; Part 1
(iterate (for (v1 v2) in (cdr (apply #'concatenate 'list
                                     (iterate (for (a b) on *points1* by #'cdr)
                                       (when (and a b)
                                         (collect (iterate (for (c d) on *points2* by #'cdr)
                                                    (when (and c d)
                                                      (collect (list (list a b) (list c d)))))))))))
  (when-it (intersection-point v1 v2)
    (finding it minimizing (distance +start+ it)))) ; => (-399 . 0)
(distance +start+ '(-399 . 0)) ; => 399 (9 bits, #x18F)

;; Part 2

(defun build-stops-and-steps (vectors)
  (loop
    :with points := (list (cons (cons 0 0) 0))
    :with x := 0 :and y := 0 :with steps := 0
    :for (dir . dist) :in vectors
    :do
       (destructuring-bind (dx dy) (delta dir)
         (incf x (* dx dist))
         (incf y (* dy dist))
         (incf steps dist)
         (push (cons (cons x y) steps) points))
    :finally
       (return (nreverse points))))

(defparameter *steps1* (build-stops-and-steps (elt *input* 0)))
(defparameter *steps2* (build-stops-and-steps (elt *input* 1)))

(iterate (for (v1 v2) in (cdr (apply #'concatenate 'list
                                     (iterate (for (a b) on *steps1* by #'cdr)
                                       (when (and a b)
                                         (collect (iterate (for (c d) on *steps2* by #'cdr)
                                                    (when (and c d)
                                                      (collect (list (list a b) (list c d)))))))))))
  (when-it (intersection-point (mapcar #'car v1) (mapcar #'car v2))
    (minimizing (+ (distance (caar v1) it) (distance (caar v2) it) (cdar v1) (cdar v2))))) ; => 15678 (14 bits, #x3D3E)


;; Utils

(defun distance (a b)
  (destructuring-bind ((x1 . y1) (x2 . y2)) (list a b)
    (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(defun between (k l m)
  (and (or (<= k m l) (<= l m k)) m))

(defun direction (vec)
  (let ((from (first vec))
        (to   (second vec)))
    (cond
      ((= (car from) (car to))
       'V)
      (t
       'H))))

(defun minpoint (x y)
  (if (and x y)
      (if (< (abs x) (abs y))
          x
          y)
      (or x y)))

(defun intersection-point (v1 v2)
  (destructuring-bind (((v1x1 . v1y1) (v1x2 . v1y2)) ((v2x1 . v2y1) (v2x2 . v2y2))) (list v1 v2)
    (trivia:match (list (direction v1) (direction v2))
      ((list 'H 'H)
       (when (= v1y1 v2y1)
         (when-it (minpoint (between v1x1 v1x2 v2x1) (between v1x1 v1x2 v2x2))
           (cons it v1y1))))
      ((list 'V 'V)
       (when (= v1x1 v2x1)
         (when-it (minpoint (between v1y1 v1y2 v2y1) (between v1y1 v1y2 v2y2))
           (cons v1x1 it))))
      ((list 'V 'H)
       (let ((x (between v2x1 v2x2 v1x1))
             (y (between v1y1 v1y2 v2y1)))
         (and x y(cons x y))))
      ((list 'H 'V)
       (let ((x (between v1x1 v1x2 v2x1))
             (y (between v2y1 v2y2 v1y1)))
         (and x y (cons x y)))))))
