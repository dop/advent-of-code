(defpackage #:day01
  (:use :cl)
  (:import-from #:rutils #:->> #:% #:read-file)
  (:import-from #:dop #:fn))

(in-package #:day01)

(defparameter *entries*
  (->> (aoc:puzzle 1 2020)
       (str:lines)
       (mapcar #'parse-integer)
       (sort % #'<)))

(loop named main for (a . rest) on *entries* do
  (loop for b in rest
        if (= 2020 (+ a b))
          do (return-from main (list (* a b) a b)))) ;; (1007331 1123 897)

(loop named main for (a . arest) on *entries* do
  (loop for (b . brest) on arest do
    (loop for c in brest
          if (= 2020 (+ a b c))
            do (return-from main (list (* a b c) a b c))))) ;; (48914340 1361 60 599)

;; double pointer search
(let ((arr (coerce *entries* 'vector)))
  (loop
    with i = 0 and j = (1- (length arr))
    while (< i j) do
      (let* ((a @arr[i])
             (b @arr#j)
             (sum (+ a b)))
        (cond ((= sum 2020)
               (return (* a b)))
              ((< sum 2020)
               (incf i))
              (t
               (decf j)))))) ;; 1007331
