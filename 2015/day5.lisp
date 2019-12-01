(ql:quickload :rutils)

(cl:defpackage #:aoc201505 (:use #:cl #:rutils))

(cl:in-package #:aoc201505)

(defparameter *input* (split-sequence #\newline (string-trim '(#\newline) (slurp "day5.txt"))))

(defun is-vowel-p (c)
  (case c
    ((#\a #\e #\i #\o #\u) t)
    (t nil)))

(defun nice-string (input)
  (loop
    :for p := nil :then c
    :for c :across input
    :count (is-vowel-p c) :into vowels
    :count (and p (char= p c)) :into doubles
    :never (and p (member (cons p c) '((#\a . #\b) (#\c . #\d) (#\p . #\q) (#\x . #\y))
                          :test #'equal))
    :finally
       (return (and (> vowels 2)
                    (plusp doubles)))))

(defun rule1 (word)
    (loop :with rule1 := (make-hash-table :test 'equal)
          :for i :from 0
          :for (a b) :on word :do
            (when (and a b)
                (if-it (gethash (cons a b) rule1)
                       (when (> (- i it) 1)
                           (return t))
                       (setf (gethash (cons a b) rule1) i)))))

(defun rule2 (word)
    (loop :for (a b c) :on word
            :thereis (and a b c (char= a c))))

(defun nice-string-2 (input)
    (let ((word (coerce input 'list)))
        (and (rule1 word) (rule2 word))))

(count-if #'nice-string *input*) ;; => 236
(count-if #'nice-string-2 *input*) ;; => 51
