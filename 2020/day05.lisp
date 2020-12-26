(defpackage #:day05
  (:use #:cl)
  (:import-from #:named-readtables #:in-readtable)
  (:import-from #:dop #:fn)
  (:import-from #:rutils #:% #:it #:-> #:->>))

(in-package #:day05)

(in-readtable :aoc)

(defparameter +input+
  (->> (aoc:puzzle 5) (str:lines)))

(defun lower-split (x y)
  (subseq (dop:binary-split x y) 0 2))

(defun upper-split (x y)
  (subseq (dop:binary-split x y) 2))

(defun seat (pass)
  (list
   (-> (subseq pass 0 7)
       (reduce (fn ((l r) #\F) (lower-split l r)
                 ((l r) #\B) (upper-split l r))
               % :initial-value '(0 127))
       (first))
   (-> (subseq pass 7)
       (reduce (fn ((l r) #\L) (lower-split l r)
                 ((l r) #\R) (upper-split l r))
               % :initial-value '(0 7))
       (first))))

(defun seat-id (pass)
  (destructuring-bind (r c) (seat pass)
    (+ (* r 8) c)))

(defun seat-id-v2 (pass)
  (->> pass
       (dop:translate '((#\F #\0) (#\B #\1) (#\R #\1) (#\L #\0)))
       (parse-integer % :radix 2)))

;; Part 1

(reduce #'max (mapcar #'seat-id-v2 +input+)) ;; 883



;; Part 2

(loop
  for pr = r and pc = c
  for (r c) in (->> (mapcar #'seat +input+)
                    (stable-sort % #'< :key #'second)
                    (stable-sort % #'< :key #'first))
  do
     (when (and pr pc
                (not (= pr (1- r)))
                (not (= pc (1- c))))
       (return (list pr pc r c)))) ;; (66 3 66 5)

(+ (* 8 66) 4) ;; 532

(->> (mapcar #'seat-id-v2 +input+)
     (sort % #'<)
     (loop for (p n) on % if (and p (> (- n p) 1)) return (1- n))) ;; 532
