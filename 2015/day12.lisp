(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201512
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:if-it #:getsethash))

(in-package #:aoc201512)

(defun pairp (x)
  (and (consp x) (symbolp (car x))))

(defun walktree (fn tree)
  (cond
    ((consp tree)
     (let ((x (car tree)))
       (unless (and (pairp x)
                    (find "red" tree :test #'equalp :key #'cdr))
         (walktree fn x)
         (walktree fn (cdr tree)))))
    (t
     (funcall fn tree))))

(defun sumtree (tree)
  (let ((sum 0))
    (walktree (lambda (x)
                (when (numberp x)
                  (incf sum x)))
              tree)
    sum))

(with-open-file (in "day12.json")
  (sumtree (cl-json:decode-json in)))
;; 65402
;; 111754
