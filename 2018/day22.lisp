(cl:defpackage #:aoc201822
  (:use #:cl #:rutils #:optima))

(cl:in-package #:aoc201822)

;; depth: 9465
;; target: 13,704

(defparameter *depth* 9465)
(defparameter *target* (cons 13 704))
(defparameter *erosion-cache* (make-hash-table :test 'equal))

(defun geo-index (x y)
  (cond ((and (= x 0) (= y 0))
         0)
        ((and (= x (car *target*))
              (= y (cdr *target*)))
         0)
        ((= y 0) (* x 16807))
        ((= x 0) (* y 48271))
        (t
         (* (erosion-level (1- x) y)
            (erosion-level x (1- y))))))

(defun erosion-level (x y)
  (rutils:getsethash (cons x y) *erosion-cache*
                     (rem (+ (geo-index x y) *depth*) 20183)))

(defun region-risk (x y)
  (rem (erosion-level x y) 3))

(defun region-type (x y)
  (case (region-risk x y)
    (0 'rocky)
    (1 'wet)
    (2 'narrow)))

(let ((*depth* 510)
      (*target* (cons 10 10)))
  (let ((x 10)
        (y 10))
    (values (geo-index x y)
            (erosion-level x y)
            (region-type x y))))

(let ((sum 0))
  (loop :for y :from 0 :to (cdr *target*) :do
    (loop :for x :from 0 :to (car *target*) :do
      (incf sum (region-risk x y))))
  sum) ;; => 9940

(defun valid-equipment-for (region-type)
  (ecase region-type
    (rocky '(torch climbing))
    (wet '(neither climbing))
    (narrow '(torch neither))))

(defun region-nodes (x y)
  (let ((nodes))
    (loop :for i :from 0 :to x :do
      (loop :for j :from 0 :to y :do
        (loop :for equipment :in (valid-equipment-for (region-type i j)) :do
          (push (list i j equipment) nodes))))
    (fset:convert 'fset:set nodes)))

(defun equipment-change-cost (equip1 equip2)
  (if (eq equip1 equip2) 0 7))

(defun move-costs (from-type to-type equipment)
  (loop :for new-equipment :in (valid-equipment-for to-type)
        :collect (cons new-equipment (1+ (equipment-change-cost equipment new-equipment)))))

(defun inside-p (x y max-x max-y)
  (and (>= x 0) (>= y 0) (<= x max-x) (<= y max-y)))

(defun remove-outside (coords max-x max-y)
  (remove-if-not (lambda (xy) (inside-p (car xy) (cdr xy) max-x max-y)) coords))

(defun calculate-distances (max-x max-y)
  (let* ((unvisited (region-nodes max-x max-y))
         (distances (make-hash-table :test 'equal))
         (current-equipment 'torch))
    (setf (gethash (list 0 0 current-equipment) distances) 0)
    (labels ((pop-closest-node ()
               ;; (format t "~a~%" (hash-table-to-alist distances))
               ;; (format t "~a~%" unvisited)
               (loop
                 :with min-distance :and min-node
                 :for node :being :the :hash-keys :of distances :using (:hash-value distance) :do
                   (when (and (fset:contains? unvisited node)
                              (or (not min-distance) (< distance min-distance)))
                     (setf min-distance distance
                           min-node node))
                 :finally
                    (setf unvisited (fset:less unvisited min-node))
                    (return (values min-node min-distance)))))
      (loop :until (fset:empty? unvisited) :do
        ;; (format t "~5d~%" (fset:size unvisited))
        (multiple-value-bind (node distance) (pop-closest-node)
          ;; (format t "~a ~a~%" node distance)
          (destructuring-bind (x y equipment) node
            (loop :for (nx . ny) :in (remove-outside (adjacent x y) max-x max-y) :do
              (loop :for (nequipment . cost) :in (move-costs (region-type x y) (region-type nx ny) equipment) :do
                (let ((new-distance-to-neighbour (+ cost distance))
                      (old-distance-to-neighbour (gethash (list nx ny nequipment) distances)))
                  (unless (and old-distance-to-neighbour
                               (< old-distance-to-neighbour new-distance-to-neighbour))
                    (setf (gethash (list nx ny nequipment) distances) new-distance-to-neighbour)))))))))
    distances))

(defun adjacent (x y)
  (list (cons (1- x) y)
        (cons (1+ x) y)
        (cons x (1- y))
        (cons x (1+ y))))

(assoc '(13 704 torch) (hash-table-to-alist (calculate-distances 13 704)) :test #'equal) ;; => ((13 704 TORCH) . 958)
(assoc '(13 704 torch) (hash-table-to-alist (calculate-distances 21 715)) :test #'equal) ;; => ((13 704 TORCH) . 942)

;; 962 too high
;; 942 too low

(let ((*depth* 9465)
      (*target* (cons 704 13))
      (*erosion-cache* (make-hash-table :test 'equal)))
  (assoc '(704 13 torch) (hash-table-to-alist (calculate-distances 715 21)) :test #'equal))
;; => 962

(let ((*depth* 9465)
      (*target* (cons 13 704))
      (*erosion-cache* (make-hash-table :test 'equal)))
  (assoc '(13 704 torch) (hash-table-to-alist (calculate-distances 18 709)) :test #'equal))
;; => ((13 704 TORCH) . 944)

(defparameter *distances* (calculate-distances 21 715))

(let ((*depth* 510)
      (*target* (cons 10 10))
      (*erosion-cache* (make-hash-table :test 'equal)))
  (assoc '(10 10 torch) (hash-table-to-alist (calculate-distances 30 30)) :test #'equal))
