(defpackage #:day22
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry))

(in-package #:day22)

(defmacro while-it (expr &body body)
  `(loop for it = ,expr while it do ,@body))

(defparameter *example*
  "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defun parse-input (input)
  (destructuring-bind (p1 p2) (cl-ppcre:split #?"\n\n" input)
    (list (mapcar #'parse-integer (cdr (str:lines p1)))
          (mapcar #'parse-integer (cdr (str:lines p2))))))

(parse-input *example*)

(defun solve1 (input)
  (destructuring-bind (p1 p2) (parse-input input)
    (loop for i from 0 while (and p1 p2 (< i 1000)) do
      (let ((c1 (pop p1))
            (c2 (pop p2)))
        (if (< c1 c2)
            (appendf p2 (list c2 c1))
            (appendf p1 (list c1 c2)))))
    (result (or p1 p2))))

(defun result (cards)
  (loop for i from (length cards) downto 1
        sum (* i (pop cards))))

(solve1 (dop:puzzle)) ;; 32677

(defun play (p1 p2)
  (let ((m1 (make-hash-table :test 'equal))
        (m2 (make-hash-table :test 'equal))
        (i 0))
    (loop while (and p1 p2 (< (incf i) 1000000)) do
      ;; (format t "~3d: ~a ~a ~a ~a~%" i p1 (result p1) p2 (result p2))
      (when (gethash p1 m1)
        (return-from play (values p1 p2)))
      (when (gethash p2 m2)
        (return-from play (values p1 p2)))
      (setf (gethash p1 m1) t
            (gethash p2 m2) t)

      (let ((c1 (pop p1))
            (c2 (pop p2)))
        (if (and (<= c1 (length p1))
                 (<= c2 (length p2)))
            (if (play (subseq p1 0 c1) (subseq p2 0 c2))
                (appendf p1 (list c1 c2))
                (appendf p2 (list c2 c1)))
            (if (> c1 c2)
                (appendf p1 (list c1 c2))
                (appendf p2 (list c2 c1))))))

    (when (>= i 1000000)
      (error "Loop limit reached."))

    (return-from play (values p1 p2))))

(defun solve2 (input)
  (destructuring-bind (p1 p2) (parse-input input)
    (result (car (remove nil (multiple-value-list (play p1 p2)))))))

(solve2 (dop:puzzle)) ;; 33661
