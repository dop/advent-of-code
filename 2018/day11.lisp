(ql:quickload '(:rutils :dlist))
(cl:defpackage #:aoc-2018-day11 (:use #:cl))
(cl:in-package #:aoc-2018-day11)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *print-circle* t))

(defparameter *grid*
  (namap (make-array '(300 300))
         (lambda (x y)
           (power-level x y 8868))))

;; (loop :for x :from 0 :below 300 :do
;;   (loop :for y :from 0 :below 300 :do
;;     (format t "~a" (aref *grid* x y)))
;;   (format t "~%"))

(defparameter *serial* 8868)

(defparameter *summed-area-table*
  (make-hash-table :test 'equal))

(defun power-level (x y &optional (serial-number *serial*))
  (let ((rack-id (+ x 10)))
    (- (rem (floor (/ (* rack-id (+ serial-number (* y rack-id)))
                      100)) 10)
        5)))

;; fill
(loop :for x :from 1 :to 300 :do
  (loop :for y :from 1 :to 300 :do
    (setf (gethash (cons x y) *summed-area-table*)
          (+ (gethash (cons (- x 1) y) *summed-area-table* 0)
              (gethash (cons x (- y 1)) *summed-area-table* 0)
              (power-level x y)
              (- (gethash (cons (- x 1) (- y 1)) *summed-area-table* 0))))))

(defun area-sum (x y size)
  (let ((w (1- size)))
    (-  (+  (gethash (cons (- x 1) (- y 1)) *summed-area-table* 0)
            (gethash (cons (+ x w) (+ y w)) *summed-area-table* 0))
        (gethash (cons (+ x w) (- y 1)) *summed-area-table* 0)
        (gethash (cons (- x 1) (+ y w)) *summed-area-table* 0))))

(let ((max-power 0) coords)
  (loop :for x :from 1 :to 298 :do
    (loop :for y :from 1 :to 298 :do
      (let ((power (area-sum x y 3)))
        (when (< max-power power)
          (setf max-power power)
          (setf coords (cons x y))))))
  (list max-power coords)) ;; => (30 (241 . 40))

;; => (30 (241 . 40))

(let ((*serial* 8868) (max-power 0) coords msize)
  (loop :for size :from 2 :to 300 :do
    (loop :for x :from 1 :to (- 301 size) :do
      (loop :for y :from 1 :to (- 301 size) :do
        (let ((power (area-sum x y size)))
          (when (< max-power power)
            (setf max-power power)
            (setf coords (cons x y))
            (setf msize size))))))
    ;; (format t "size ~a: ~a ~a ~a~%" size max-power msize coords)
    (list max-power coords msize)) ;; => (71 (166 . 75) 12)


(defparameter *cache*
  (make-hash-table :test 'equal))

(defun mtotal-power (x y size &optional (serial-number *serial*))
  (+ (or (gethash (list x y (1- size) serial-number) *cache*)
         (total-power x y (1- size) serial-number))
      (power-level (+ x (1- size)) (+ y (1- size)) serial-number)
      (loop :for dx :from 0 :below (1- size) :sum (power-level (+ x dx) (+ y (1- size)) serial-number))
      (loop :for dy :from 0 :below (1- size) :sum (power-level (+ x (1- size)) (+ y dy) serial-number))))

(defun total-power (x y size &optional (serial-number *serial*))
  (let ((sum 0))
    (loop :for dx :from 0 :below size :do
      (loop :for dy :from 0 :below size :do
        (incf sum (power-level (+ x dx) (+ y dy) *serial*))))
    sum))

(let ((*serial* 8868)
      (max-power 0)
      coords)
  (loop :for x :from 1 :to 298 :do
    (loop :for y :from 1 :to 298 :do
      (let ((power (total-power x y 3)))
        (when (< max-power power)
          (setf max-power power)
          (setf coords (cons x y))))))
  (list max-power coords))

;; => (30 (241 . 40))

;; This won't finish running, but because of luck it gave right answer pretty
;; soon. Didn't really help.
(let ((*serial* 8868) (max-power 0) coords msize)
  (loop :for size :from 2 :to 300 :do
    (loop :for x :from 1 :to (- 301 size) :do
      (loop :for y :from 1 :to (- 301 size) :do
        (let ((power (mtotal-power x y size)))
          (when (< max-power power)
            (setf max-power power)
            (setf coords (cons x y))
            (setf msize size)))))
    (format t "size ~a: ~a ~a ~a~%" size max-power msize coords)
    (list max-power coords msize)))

;; 166,75,12
