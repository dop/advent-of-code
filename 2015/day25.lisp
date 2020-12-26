(defpackage #:day25-2015
  (:use #:cl #:iterate)
  (:import-from #:dop #:fn #:kw)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:->> #:% #:xor #:it)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day25-2015)

;; Enter the code at row 2947, column 3029.

(defun solve (r c start)
  (let ((num start))
    (loop repeat (1- (+ (loop for i from 1 below (1- (+ r c)) sum i) c)) do
      (setf num (rem (* num 252533) 33554393)))
    num))

(solve 2947 3029 20151125) ;; 19980801
