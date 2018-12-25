(cl:defpackage #:aoc201505
  (:use #:cl #:rutils #:optima))

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

(defun nice-string-2 (input)
  (loop
    :with pairs := (make-hash-table :test 'equal)
    :for pp := nil :then p
    :for p := nil :then c
    :for c :across input
    :for i :from 0
    :do (when p (push i (gethash (cons p c) pairs)))
    :count (and pp (char= pp c)) :into separated-doubles
    :finally
       (let ((apairs (rutils:hash-table-to-alist pairs)))
         (return (and (plusp separated-doubles)
                      (some (lambda (pair) (and (> (length (cdr pair)) 1) (not-close (cdr pair)))) apairs)
                      (notany (lambda (pair) (and (> (length (cdr pair)) 1) (not (not-close (cdr pair))))) apairs))))))

(nice-string-2 "axa")

(defun not-close (list)
  (loop :for (a b) :on list :by #'cdr
        :always (if (and a b) (> (abs (- a b)) 1) t)))

(loop :for string :in *input* :count (nice-string string))
