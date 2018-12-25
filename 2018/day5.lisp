(load "~/quicklisp/setup.lisp")
(ql:quickload :rutils)
(defpackage #:aoc-2018-day5 (:use #:cl #:rutils))
(in-package #:aoc-2018-day5)

(defconstant *polymer* (string-trim '(#\newline) (rutils:slurp "input5.txt")))

(defun destroy? (a b)
  (and a b (char/= a b) (char-equal a b)))

(defun reduce-polymer (polymer)
  (let ((stack (list (elt polymer 0))))
    (loop :for u :across (subseq polymer 1)
          :do
             (push u stack)
             (if (destroy? (car stack) (cadr stack))
                 (progn
                   (pop stack)
                   (pop stack))))
    stack))

(length (reduce-polymer *polymer*)) ;; => 11476

(defun reduce-polymer2 (polymer1 unit-to-skip)
  (let ((polymer (remove-if (lambda (c) (char-equal c unit-to-skip)) polymer1)))
    (reduce-polymer polymer)))

(loop :for i :from (char-code #\a) :to (char-code #\z)
      :minimizing (length (reduce-polymer2 *polymer* (code-char i)))) ;; => 5446
