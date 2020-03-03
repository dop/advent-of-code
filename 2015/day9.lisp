(ql:quickload '(:alexandria :rutils :str))

(defpackage #:aoc201509
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:if-it #:getsethash))

(in-package #:aoc201509)

(defparameter *example*
  "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")

(defparameter *input*
  "Faerun to Tristram = 65
Faerun to Tambi = 129
Faerun to Norrath = 144
Faerun to Snowdin = 71
Faerun to Straylight = 137
Faerun to AlphaCentauri = 3
Faerun to Arbre = 149
Tristram to Tambi = 63
Tristram to Norrath = 4
Tristram to Snowdin = 105
Tristram to Straylight = 125
Tristram to AlphaCentauri = 55
Tristram to Arbre = 14
Tambi to Norrath = 68
Tambi to Snowdin = 52
Tambi to Straylight = 65
Tambi to AlphaCentauri = 22
Tambi to Arbre = 143
Norrath to Snowdin = 8
Norrath to Straylight = 23
Norrath to AlphaCentauri = 136
Norrath to Arbre = 115
Snowdin to Straylight = 101
Snowdin to AlphaCentauri = 84
Snowdin to Arbre = 96
Straylight to AlphaCentauri = 107
Straylight to Arbre = 14
AlphaCentauri to Arbre = 46")

(defun parse-line (line)
  (destructuring-bind (direction distance) (str:split " = " line)
    (destructuring-bind (from to) (str:split " to " direction)
      (list from to (parse-integer distance)))))

(defun parse-input (input)
  (loop
    :with cities := '()
    :with distances := (make-hash-table :test 'equalp)
    :for (from to distance) :in (mapcar #'parse-line (str:lines input))
    :do (setf (gethash (cons from to) distances) distance
              (gethash (cons to from) distances) distance)
        (pushnew from cities :test #'equalp)
        (pushnew to cities :test #'equalp)
    :finally
       (return (values distances cities))))

(multiple-value-bind (distances cities) (parse-input *input*)
  (loop :for path :in (permutations cities)
        :maximize (loop :for (from to) :on path :when (and from to)
                        :sum (gethash (cons from to) distances))))
