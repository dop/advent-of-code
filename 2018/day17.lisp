(ql:quickload '(:rutils :optima :cl-ppcre))
(cl:defpackage #:aoc-2018-day17
  (:use #:cl #:optima #:cl-ppcre)
  (:import-from #:rutils #:hash-table-from-alist #:hash-table-to-alist #:slurp))
(cl:in-package #:aoc-2018-day17)

(defparameter *example*
  "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=505, y=10..13
y=13, x=498..500
y=13, x=502..502
y=13, x=504..505")

(defun to-coordinate (name from to)
  (list (intern (string-upcase name))
        (if (and from to)
            (range (parse-integer from) (parse-integer to))
            (list (parse-integer from)))))

(defun parse-coordinates (input)
  (let (coordinates)
    (do-register-groups (var1 from1 _1 to1 var2 from2 _2 to2)
        ("(.)=(\\d+)(\\.\\.(\\d+))?, (.)=(\\d+)(\\.\\.(\\d+))?" input)
      (push (nconc (to-coordinate var1 from1 to1)
                   (to-coordinate var2 from2 to2))
            coordinates))
    (nreverse coordinates)))

(defun read-world (input)
  (loop
    :with clay := (make-hash-table :test 'equal)
    :for coords :in (parse-coordinates input)
    :minimize (car (getf coords 'x)) :into left
    :maximize (car (last (getf coords 'x))) :into right
    :minimize (car (getf coords 'y)) :into top
    :maximize (car (last (getf coords 'y))) :into bottom
    :do
       (loop :for x :in (getf coords 'x) :do
         (loop :for y :in (getf coords 'y) :do
           (setf (gethash (cons x y) clay) t)))
    :finally
       (return (list clay left right top bottom))))

(defparameter *world* (read-world *example*))
(defparameter *real-world* (read-world (slurp "day17.txt")))

(defun print-world (world &optional water path)
  (destructuring-bind (clay left right top bottom) world
    (loop :for y :from (1- top) :to (1+ bottom) :do
      (loop :for x :from (1- left) :to (1+ right) :do
        (cond ((and (= y 0) (= x 500)) (write-char #\+))
              ((gethash (cons x y) clay) (write-char #\#))
              ((and water (gethash (cons x y) water)) (write-char #\~))
              ((and path (gethash (cons x y) path)) (write-char #\|))
              (t (write-char #\.))))
      (write-char #\newline))))

(defparameter *clay* nil)
(defparameter *water* nil)

(defun is-clay-p (x y)
  (gethash (cons x y) *clay*))

(defun is-water-p (x y)
  (gethash (cons x y) *water*))

(defun is-sand-p (x y)
  (not (or (is-clay-p x y) (is-water-p x y))))

(defun set-water (x y)
  (setf (gethash (cons x y) *water*) t))

(defun set-path (x y)
  (setf (gethash (cons x y) *path*) t))

(defun escape (direction ox oy)
  (loop
    :with y := oy
    :for x := (+ ox direction) :then (+ x direction)
    :until (is-clay-p x y)
    :do
       (set-path x y)
       (when (is-sand-p x (1+ y)) (return (cons x y)))
    :count t :into count
    :finally (return count)))

(defun flow (x y world water path escapes)
  (setf (gethash (cons x y) escapes) t)
  (let ((*clay* (first world))
        (*water* water)
        (*path* path)
        (bottom (fifth world)))
    (loop
      :do
         (incf y)
         ;; (format t "~a,~a~%" x y)
         (unless (or (is-clay-p x y) (is-water-p x y))
           (set-path x y))
         (when (> y bottom)
           (return-from flow))
      :until (or (is-clay-p x y) (is-water-p x y))
      :finally (decf y))

    ;; (format t "hit bottom at ~a,~a~%" x y)

    (let (right left)
      (ematch (escape 1 x y)
        ((cons x y)
         ;; (format t "can escape right at ~a,~a~%" x y)
         (unless (gethash (cons x y) escapes)
           (flow x y world water path escapes)))
        ((guard x (numberp x))
         (setf right x)))
      (ematch (escape -1 x y)
        ((cons x y)
         ;; (format t "can escape left at ~a,~a~%" x y)
         (unless (gethash (cons x y) escapes)
           (flow x y world water path escapes)))
        ((guard x (numberp x))
         (setf left x)))
      ;; (format t "left ~a, right ~a~%" left right)
      (when (and left right)
        ;; (format t "can fill this space: ~a~%" (+ 1 left right))
        (set-path x y)
        (loop :for i :from (- x left) :to (+ x right) :do (set-water i y))))))

(let ((water (make-hash-table :test 'equal))
      (path  (make-hash-table :test 'equal))
      (world (read-world *example*)))
  (loop :for i :from 0 :to 10 :do
    (let ((escapes (make-hash-table :test 'equal)))
      (flow 500 0 world water path escapes)))

  (let* ((top (fourth world))
         (bottom (fifth world))
         (count (loop :for (x . y) :being :the :hash-keys :of path
                      :count (and (>= y top) (<= y bottom)))))
    (format t "water m²: ~a~%" (hash-table-count water))
    (format t "water path: ~a~%" count))
  (print-world world water path))

