(cl:defpackage #:aoc-2018-day18
  (:use #:cl #:rutils #:cl-ppcre #:optima)
  (:import-from #:dop #:amapi #:each))
(cl:in-package #:aoc-2018-day18)

(defparameter *example* ".#.#...|#......#|##|.|..|...#...|#.....##.#|||#|#|...#.||....|....|...||...#|.#||.||||..|....#.|..|.")

(defparameter *input* (string-trim '(#\newline) (slurp "day18.txt")))

(defparameter *width* 50)
(defparameter *height* 50)
(defparameter *outskirts* (make-array '(50 50)))

(loop :for y :from 0 :below *width* :do
  (loop :for x :from 0 :below *height* :do
    (setf (aref *outskirts* y x) (char *input* (+ x (* y *width*))))))

(defun adjacent (x y)
  (let (adjacent)
    (loop :for j :from (1- y) :to (1+ y) :do
      (loop :for i :from (1- x) :to (1+ x) :do
        (when (and (> i -1) (< i *width*)
                   (> j -1) (< j *height*)
                   (not (and (= i x) (= j y))))
          (push (cons i j) adjacent))))
    adjacent))

(defun @ (x y) (aref *outskirts* y x))

(defun tree-p (c)
  (char= #\| c))

(defun lumberyard-p (c)
  (char= #\# c))

(defun open-p (c)
  (char= #\. c))

(defun evolve (x y)
  (loop :for (i . j) :in (adjacent x y)
        :for acre := (@ i j)
        :count (open-p acre) :into open
        :count (lumberyard-p acre) :into lumberyards
        :count (tree-p acre) :into trees
        :finally
           (return
             (let ((current (@ x y)))
               (cond
                 ((and (open-p current) (> trees 2)) #\|)
                 ((and (tree-p current) (> lumberyards 2)) #\#)
                 ((and (lumberyard-p current) (plusp lumberyards) (plusp trees)) #\#)
                 ((lumberyard-p current) #\.)
                 (t current))))))

(defun evolve (x y)
  (loop :for (i . j) :in (adjacent x y)
        :for acre := (@ i j)
        :count (open-p acre) :into open
        :count (lumberyard-p acre) :into lumberyards
        :count (tree-p acre) :into trees
        :finally
           (return
             (let ((current (@ x y)))
               (list current open lumberyards trees)))))

(defun evolve-outskirts (outskirts)
  (let ((*outskirts* outskirts))
    (amapi outskirts (lambda (y x) (evolve x y)))))

(loop :for i :below (+ 471 25 1)
      :for outskirts := *outskirts* :then (evolve-outskirts outskirts)
      :finally
         (print-outskirts outskirts)
         (let ((lumberyards 0)
               (trees 0)
               (*outskirts* outskirts))
           (each *outskirts* (lambda (y x)
                               (when (lumberyard-p (@ x y))
                                 (incf lumberyards))
                               (when (tree-p (@ x y))
                                 (incf trees))))
           (format t "~A~%" (list :lumberyards lumberyards
                                  :trees trees
                                  :result (* lumberyards trees)))))

;; iteration: 471, previous: 443
;; (LUMBERYARDS 333 TREES 585 RESULT 194805)

443 = 471

(rem (- 1000000000 443) 28)

;; => 202301

(defun to-string (outskirts)
  (let ((*outskirts* outskirts))
    (with-output-to-string (out)
      (each outskirts (lambda (y x) (write-char (@ x y) out))))))

(loop
  :with seen := (make-hash-table :test 'equal)
  :for i :below 1000
  :for outskirts := *outskirts* :then (evolve-outskirts outskirts)
  :until (gethash (to-string outskirts) seen)
  :do (setf (gethash (to-string outskirts) seen) i)
  :finally
     (print-outskirts outskirts)
     (let ((lumberyards 0)
           (trees 0)
           (*outskirts* outskirts))
       (each *outskirts* (lambda (y x)
                           (when (lumberyard-p (@ x y))
                             (incf lumberyards))
                           (when (tree-p (@ x y))
                             (incf trees))))
       (format t "iteration: ~A, previous: ~A~%~A~%"
               i
               (gethash (to-string outskirts) seen)
               (list :lumberyards lumberyards
                     :trees trees
                     :result (* lumberyards trees)))))



(defun print-outskirts (outskirts)
  (let ((*outskirts* outskirts))
    (each outskirts
          (lambda (y x)
            (format t "~A" (@ x y))
            (when (= x (1- *width*))
              (format t "~%"))))))
