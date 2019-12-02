(in-package :advent-of-code-2019-day1)

(defparameter *input*
  (mapcar #'parse-integer (str:lines (rutils:slurp "day01.txt"))))

(defun fuel-requirement (mass)
  (- (floor mass 3) 2))

;; What is the sum of the fuel requirements for all of the modules on your
;; spacecraft?
(loop :for mass :in *input* :sum (fuel-requirement mass)) ; => 3297866 (22 bits, #x32524A)

(defun fixed-fuel-requirement (mass)
  (loop :for fuel := (fuel-requirement mass) :then (fuel-requirement fuel)
        :while (plusp fuel)
        :sum fuel))

;; What is the sum of the fuel requirements for all of the modules on your
;; spacecraft when also taking into account the mass of the added fuel?
;; (Calculate the fuel requirements for each module separately, then add them
;; all up at the end.)
(loop :for mass :in *input* :sum (fixed-fuel-requirement mass)) ; => 4943923 (23 bits, #x4B7033)
