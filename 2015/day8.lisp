(ql:quickload '(:alexandria :rutils :str))

(defpackage #:aoc201508
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:if-it #:getsethash))

(in-package #:aoc201508)

(defparameter *example*
  "\"\"
\"abc\"
\"aaa\\\"aaa\"
\"\\x27\"")

(defun lines (input)
  (mapcar (lambda (line) (subseq line 1 (1- (length line))))
          (str:lines input)))

(defun count-characters (line)
  (loop
    :with len := (length line)
    :for i :from 0 :below len
    :for c := (elt line i)
    :count t
    :do (when (eq c #\\)
          (case (elt line (1+ i))
            ((#\\ #\") (incf i))
            (#\x (incf i 3))))))

(loop :for line :in (lines *input*)
      :sum (+ 2 (- (length line) (count-characters line))))

(defun count-escaped-characters (line)
  (loop
    :with count := 6
    :with len := (length line)
    :for i :from 0 :below len
    :for c := (elt line i)
    :do (cond
          ((eq c #\\)
           (case (elt line (1+ i))
             (#\x
              (incf i 3)
              (incf count 5))
             (t
              (incf i)
              (incf count 4))))
          (t
           (incf count)))
    :finally
       (return count)))

(loop :for line :in (lines *input*)
      :sum (- (count-escaped-characters line) (+ 2 (length line))))
