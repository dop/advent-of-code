(defpackage #:day25
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry))

(in-package #:day25)

(named-readtables:in-readtable :aoc)

(defun next-value (subject-number current-value)
  (rem (* current-value subject-number) 20201227))

(defun transform-subject-number (subject-number loop-size)
  (loop with value = 1
        repeat loop-size
        do
           (setf value (next-value subject-number value))
        finally
           (return value)))

(defun find-loop-size (pubkey)
  (loop with value = 1
        for i from 1 below 100000000
        do (let ((nv (next-value 7 value)))
             (when (= pubkey nv)
               (return i))
             (setf value nv))))

(defun solve1 (input)
  (destructuring-bind (cpk dpk) (mapcar #'parse-integer (str:words input))
    (let* ((cls (find-loop-size cpk))
           (dls (find-loop-size dpk))
           (csn (transform-subject-number 7 cls))
           (dsn (transform-subject-number 7 dls))
           (ek1 (transform-subject-number csn dls))
           (ek2 (transform-subject-number dsn cls)))
      (when (= ek1 ek2)
        ek1))))
(time
 (solve1 "8184785 5293040"))
 ;; 4126980
