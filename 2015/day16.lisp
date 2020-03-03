(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201516
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201516)

(defun read-input ()
  (loop :for (_ i . props) :in (mapcar (lambda (line) (str:split " " (remove-if (lambda (c) (member c '(#\: #\,))) line))) (str:lines (rutils:slurp "day16.txt")))
        :collect (cons (parse-integer i) (loop :for (prop value) :on props :by #'cddr :collect (cons (intern (string-upcase prop)) (parse-integer value))))))

(defparameter *input* (read-input))

(defparameter *match*
  '((children . 3)
    (cats . 7)
    (samoyeds . 2)
    (pomeranians . 3)
    (akitas . 0)
    (vizslas . 0)
    (goldfish . 5)
    (trees . 3)
    (cars . 2)
    (perfumes . 1)))

(defun match? (props)
  (loop :for (prop . value) :in props
        :always (let ((target-value (cdr (assoc prop *match*))))
                  (case prop
                   ((cats trees)
                    (> value target-value))
                   ((pomeranians goldfish)
                    (< value target-value))
                   (t
                    (eq value target-value))))))

(remove-if-not (compose #'match? #'cdr) *input*)
;; ((373 (POMERANIANS . 3) (PERFUMES . 1) (VIZSLAS . 0)))
