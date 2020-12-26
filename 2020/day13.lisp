(defpackage #:day13
  (:use #:cl))

(in-package #:day13)

(destructuring-bind (arrival-string departures-string) (str:lines (aoc:puzzle 13))
  (let ((arrival (parse-integer arrival-string))
        (departures (mapcar #'parse-integer (remove-if (lambda (x) (string= "x" x))
                                                       (str:split "," departures-string)))))
    (let ((bid  )
          (best-wait (reduce #'max departures)))
      (loop for id in departures do
        (let ((wait (- id (nth-value 1 (floor arrival id)))))
          (when (< wait best-wait)
            (setf bid id
                  best-wait wait))))
      (* bid best-wait)))) ;; 4315

(defun egcd (a b)
  (let ((x 0) (y 1) (u 1) (v 0))
    (loop while (not (zerop a)) do
      (multiple-value-bind (q r) (floor b a)
        (psetf b a
               a r
               x u
               y v
               u (- x (* u q))
               v (- y (* v q)))))
    (values b x y)))

(defun find-chinese-remainder (moduli remainders)
  "Solves equation for x, where MODULI is mₙ and REMAINDERS -- aₙ.

   x = a₀ mod m₀
   x = a₁ mod m₁
   ...
   x = aₙ mod mₙ"
  (let ((prod (reduce #'* moduli))
        (sum 0))
    (loop for m in moduli and r in remainders do
      (let ((prod/m (/ prod m)))
        (multiple-value-bind (_gcd inverse-prod/m _) (egcd prod/m m)
          (declare (ignore _gcd _))
          (incf sum (* r prod/m inverse-prod/m)))))
    (mod sum prod)))

(defun solve (input)
  (let* ((departures (to-departures input))
         (moduli (mapcar #'cdr departures))
         (remainders (mapcar (lambda (x) (if (zerop (car x)) 0 (- (cdr x) (car x)))) departures)))
    (find-chinese-remainder moduli remainders)))

(solve (nth 1 (str:lines (aoc:puzzle 13)))) ;; 556100168221141

(defun constraint-solve (input)
  (let ((rp 1)
        (mv 0))
    (loop for (d . m) in (to-departures input) do
      (loop while (plusp (mod (+ mv d) m)) do
        (incf mv rp))
      (setf rp (* rp m)))
    mv))

(constraint-solve (nth 1 (str:lines (aoc:puzzle 13)))) ;; 556100168221141
