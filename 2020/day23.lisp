(defpackage #:day23
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry))

(in-package #:day23)

(named-readtables:in-readtable :aoc)

(defparameter *example* "389125467")

(defstruct (mapped-list (:constructor make-mapped-list (list)))
  (list
   nil
   :type list)
  (values
   (let ((ht (make-hash-table)))
     (loop for rest = list then (cdr rest)
           while rest
           do (setf (gethash (car rest) ht) rest))
     ht)
   :type hash-table))

(defun mapped-list-move (ml start amount target-item)
  (let* ((sublist (subseq start 1 (1+ amount)))
         (target (gethash target-item (mapped-list-values ml))))
    (assert (not (member target-item sublist)))
    (let* ((end (nthcdr amount start)))
      (psetf (cdr end) (cdr target)
             (cdr target) (cdr start)
             (cdr start) (cdr end)))
    ml))

(defun solve1 (input &optional (cycles 100) (limit 9))
  (let* ((circle (map 'list #'digit-char-p input))
         (ml (make-mapped-list circle)))
    (setf (cdr (last circle)) circle)
    (labels ((next (label)
               (if (plusp (1- label)) (1- label) limit)))
      (loop for i from 1 to cycles do
        (let ((picked-up (subseq circle 1 4)))
          (loop for cup = (next (car circle)) then (next cup)
                while (member cup picked-up)
                finally
                   (mapped-list-move ml circle 3 cup)
                   (setf circle (cdr circle)))))
      (->> (mapped-list-values ml)
           (gethash 1)
           (subseq % 1 9)
           (format nil "~{~d~}" %)))))

(solve1 (str:trim (dop:puzzle 23))) ;; "96342875"

(defun solve2 (input &optional (cycles 10000000) (limit 1000000))
  (let* ((cups (map 'list #`(- (char-code %) 48) input))
         (circle (append cups (loop for i from (1+ (apply #'max cups)) to limit collect i)))
         (ml (make-mapped-list circle)))
    (setf (cdr (last circle)) circle)
    (labels ((next (label)
               (if (plusp (1- label)) (1- label) limit)))
      (loop for i from 1 to cycles do
        (let ((picked-up (subseq circle 1 4)))
          (loop for cup = (next (car circle)) then (next cup)
                while (member cup picked-up)
                finally
                   (mapped-list-move ml circle 3 cup)
                   (setf circle (cdr circle)))))

      (->> (mapped-list-values ml)
           (gethash 1 )
           (subseq % 1 3)
           (reduce #'*)))))

(time (solve2 (str:trim (dop:puzzle 23 2020)))) ;; 563362809504
