(defpackage #:day24-2015
  (:use #:cl #:iterate)
  (:import-from #:dop #:fn #:kw)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:->> #:% #:xor #:it)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day24-2015)

(defparameter +example+
  '(1 2 3 4 5 7 8 9 10 11))

(defparameter +input+
  (->> (aoc:puzzle 24 2015)
       (str:words)
       (mapcar #'parse-integer)))

(defun pack (target result choices)
  (if (plusp target)
      (let ((options) (choice (car choices)))
        (when (and choice (<= 0 (- target choice)))
          (setf options (append options
                                (pack target result (cdr choices))
                                (pack (- target choice) (cons choice result) (cdr choices)))))
        options)
      (list result)))


(let ((best-length (length +input+))
      (best-value (reduce #'* +input+)))
  (loop for choices in (pack 384 nil +input+) do
    (let ((len (length choices))
          (val (reduce #'* choices)))
      (when (and (<= len best-length) (< val best-value))
        (setf best-value val
              best-length len))))
  (values best-value best-length)) ;; 74850409

(/ (reduce #'+ +input+) 4)
