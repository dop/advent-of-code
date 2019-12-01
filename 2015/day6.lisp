(ql:quickload '(:parseq :alexandria))

(cl:defpackage #:aoc201506
  (:use #:cl #:rutils #:cl-ppcre)
  (:import-from #:alexandria #:curry #:rcurry #:compose))

(cl:in-package #:aoc201506)

(defparameter *input*
  (mapcar (curry #'parseq:parseq 'instruction)
          (split-sequence #\newline (string-trim '(#\newline) (slurp "day6.txt")))))

(subseq *input* 0 10)

(let ((lights (make-array 1000000 :initial-element (the bit 0))))
    (loop :for (action (x1 . y1) (x2 . y2)) :in *input* :do
      (loop :for x :from x1 :to x2 :do
        (loop :for y :from y1 :to y2 :do
          (let ((i (+ x (* y 1000))))
              (case action
                (on (setf (aref lights i) 1))
                (off (setf (aref lights i) 0))
                (toggle (setf (aref lights i) (rem (1+ (aref lights i)) 2))))
              ))))
    (loop :for i :from 0 :below 1000000 :sum (aref lights i))) ;; => 569999

(let ((lights (make-array 1000000 :initial-element 0)))
    (loop :for (action (x1 . y1) (x2 . y2)) :in *input* :do
      (loop :for x :from x1 :to x2 :do
        (loop :for y :from y1 :to y2 :do
          (let ((i (+ x (* y 1000))))
              (case action
                (on (incf (aref lights i)))
                (off (when (> (aref lights i) 0) (decf (aref lights i))))
                (toggle (incf (aref lights i) 2)))))))
    (loop :for i :from 0 :below 1000000 :sum (aref lights i))) ;; => 17836115

;; interesting but verbose

(parseq:defrule instruction ()
    (and action " " xy " through " xy)
    (:lambda (action _ from _ to)
        (list action from to)))

(parseq:defrule action ()
    (or turn-on turn-off toggle))

(parseq:defrule turn-on ()
    "turn on"
    (:constant 'on))

(parseq:defrule turn-off ()
    "turn off"
    (:constant 'off))

(parseq:defrule toggle ()
    "toggle"
    (:constant 'toggle))

(parseq:defrule xy ()
    (and string->number
         ","
         string->number)
    (:lambda (x _ y) (cons x y)))

(parseq:defrule string->number ()
    (rep (1 nil) digit)
    (:lambda (&rest digits)
        (loop :with result := 0 :for digit :in digits
              :do (setf result (+ (- (char-code digit) 48) (* result 10)))
              :finally (return result))))
