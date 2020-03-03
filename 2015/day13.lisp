(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201513
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201513)

(defparameter *example*
  "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
")

(defun parse-line (line)
  (with-input-from-string (in line)
    (loop :for token := (read in nil) :while token :collect token)))

(defun understand-line (line)
  (when-it (parse-line line)
    (let ((name (car it))
          (verb (car (nthcdr 2 it)))
          (amount (car (nthcdr 3 it)))
          (neighbour (car (last it))))
      (list name neighbour (if (eq verb 'lose) (- amount) amount)))))

(defun understand-input (input)
  (remove-if #'null (mapcar #'understand-line (str:split "." input))))

(defun pair-happiness (a b constraints)
  (if (or (eq 'me a) (eq 'me b))
      0
      (car (last (find (list a b) constraints :test #'equalp :key #'butlast)))))

(defun happiness (names constraints)
  (loop :for (a x) :on names
        :sum (let ((b (or x (car names))))
               (+ (pair-happiness a b constraints) (pair-happiness b a constraints)))))

(defun solve (input)
  (let* ((constraints (understand-input input))
         (names (cons 'me (remove-duplicates (mapcar #'car constraints)))))
    (values
     (loop :for seating :in (permutations names) :maximize (happiness seating constraints))
     (list names constraints))))

;; (solve (rutils:slurp "day13.txt"))
