(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201518
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201518)

(defparameter *initial*
  (make-array '(6 6) :element-type 'character :displaced-to ".#.#.#...##.#....#..#...#.#..#####.."))

(defparameter *buffer*
  (make-array '(6 6) :element-type 'character :initial-element #\.))

(defparameter *neighbour-deltas*
  '((0 1) (1 0) (1 1) (0 -1) (-1 0) (-1 -1) (1 -1) (-1 1)))

(defun neighbour-count (y x world)
  (loop :for (dx dy) :in *neighbour-deltas*
        :count (eq #\# (ignore-errors (aref world (+ y dy) (+ x dx))))))

(defun next-state (y x world)
  (case (aref world y x)
    (#\# (if (member (neighbour-count y x world) '(2 3))
           #\#
           #\.))
    (#\. (if (= 3 (neighbour-count y x world))
           #\#
           #\.))))

(defun map-state (fn world)
  (destructuring-bind (w h) (array-dimensions world)
    (loop :for y :from 0 :below h :do
      (loop :for x :from 0 :below w :do
        (funcall fn (aref world y x))))))

(defun print-state (world)
  (destructuring-bind (w h) (array-dimensions world)
    (loop :for y :from 0 :below h :do
      (loop :for x :from 0 :below w :do
        (princ (aref world y x)))
      (terpri))))

(defun advance-world (world)
  (let* ((dimensions (array-dimensions world))
         (next-world (make-array dimensions :initial-element #\.)))
    (destructuring-bind (h w) dimensions
      (loop :for y :from 0 :below h :do
        (loop :for x :from 0 :below w :do
          (setf (aref next-world y x) (next-state y x world)))))
    next-world))

(defun stuck-lights (world)
  (let* ((dimensions (array-dimensions world))
         (next-world (make-array dimensions :initial-element #\.)))
    (destructuring-bind (h w) dimensions
      (let ((corners (list (cons 0 0) (cons 0 (1- w)) (cons (1- h) 0) (cons (1- h) (1- w)))))
        (loop :for y :from 0 :below h :do
          (loop :for x :from 0 :below w :do
            (setf (aref next-world y x)
                  (if (member (cons y x) corners :test #'equalp)
                    #\#
                    (aref world y x)))))))
    next-world))

(loop :for i :from 0 :to 5
      :for world := (stuck-lights *initial*) :then (stuck-lights (advance-world world))
      :do (print-state world)
      :finally (return (let ((sum 0))
                         (map-state (lambda (x) (when (eq x #\#) (incf sum)))
                                    world)
                         sum)))

(defun read-world (input edge)
  (make-array (list edge edge) :element-type 'character :displaced-to (remove #\newline input)))

(loop :for i :from 0 :to 100
      :for world := (stuck-lights (read-world (rutils:slurp "day18.txt") 100))
        :then (stuck-lights (advance-world world))
      :finally (return (let ((sum 0))
                         (map-state (lambda (x) (when (eq x #\#) (incf sum)))
                                    world)
                         sum))) ;; 924
