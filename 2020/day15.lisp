(defpackage #:day15
  (:use #:cl #:rutils))

(in-package #:day15)

(defun solve (nth input)
  (let* ((initial (mapcar (lambda (x) (parse-integer x :junk-allowed t)) (str:split "," input)))
         (answer 0)
         (last-turn (make-hash-table)))

    (loop for i from 1 and n in initial do
      (setf (gethash n last-turn) i))

    (loop for i from (1+ (length initial)) below nth do
      (let ((num answer))
        (if (gethash num last-turn)
            (setf answer (- i (gethash num last-turn)))
            (setf answer 0))
        (setf (gethash num last-turn) i)))
    answer))

(solve 2020 (aoc:puzzle)) ;; 1111

(solve 30000000 (aoc:puzzle)) ;; 48568
