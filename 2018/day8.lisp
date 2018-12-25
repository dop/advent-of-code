(load "~/quicklisp/setup.lisp")
(ql:quickload '(:rutils :cl-ppcre))
(cl:defpackage #:aoc-2018-day8 (:use #:cl #:rutils #:cl-ppcre))
(cl:in-package #:aoc-2018-day8)

(defconstant *numbers*
  (mapcar #'read-from-string (split-sequence #\space (slurp "input8.txt"))))

(defconstant *example*
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(defun sum (numbers)
  (apply #'+ numbers))

(defun to-tree (numbers)
  (let* ((children-count (pop (car numbers)))
         (metadata-count (pop (car numbers))))
    (list
     :children (loop :for i :below children-count :collect (to-tree numbers))
     :metadata (loop :for i :below metadata-count :collect (pop (car numbers))))))

(defun sum-metadata (tree)
  (+ (sum (getf tree :metadata))
     (sum (mapcar #'sum-metadata (getf tree :children)))))

(sum-metadata (to-tree (list (copy-seq *example*)))) ;; => 138

(sum-metadata (to-tree (list (copy-seq *numbers*)))) ;; => 42472

(defun node-value (node)
  (if node
      (let ((children (getf node :children))
            (metadata (getf node :metadata)))
        (if children
            (loop :for i :in metadata :sum (node-value (nth (1- i) children)))
            (sum metadata)))
      0))

(node-value (to-tree (list (copy-seq *numbers*)))) ;; => 21810
