(defpackage #:day02
  (:use #:cl)
  (:import-from #:trivia #:match)
  (:import-from #:rutils #:xor #:%)
  (:import-from #:trivia.ppcre #:split))

(in-package #:day02)

(named-readtables:in-readtable :aoc)

(defparameter +input+
  (str:lines (aoc:puzzle 2 2020)))

(defun valid-password-p (line)
  (match line
    ((split " " (split "-" (read min) (read max)) (vector char #\:) password)
     (<= min (count-if #`(eq % char) password) max))))

(st:should be = 483 (count-if #'valid-password-p +input+))

(defun real-valid-password-p (line)
  (match line
    ((split " " (split "-" (read i) (read j)) (vector char #\:) password)
     (let ((a (char= char (elt password (1- i))))
           (b (char= char (elt password (1- j)))))
       (xor a b)))))

(st:should be = 482 (count-if #'real-valid-password-p +input+))
