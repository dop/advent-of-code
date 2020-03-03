(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201517
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201517)

(defparameter *containers*
  '(50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40))

(defun powerset (list)
  (if list
      (concatenate
       'list
       (mapcar (curry #'cons (car list)) (powerset (cdr list)))
       (powerset (cdr list)))
      '(())))

(loop :for candidate :in (powerset *containers*)
      :count (= 150 (apply #'+ candidate))) ;; 654

(loop
  :with counts := (make-hash-table)
  :for candidate :in (powerset *containers*)
  :when (= 150 (apply #'+ candidate))
  :do (getsethash (length candidate) counts 0)
      (incf (gethash (length candidate) counts))
  :finally
     (return (sort (rutils:ht->pairs counts) #'< :key #'car)))
;; ((4 57) (5 231) (6 262) (7 97) (8 7))
