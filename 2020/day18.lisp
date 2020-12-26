(defpackage #:day18
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:map-product))

(in-package #:day18)

(named-readtables:in-readtable :aoc)

(defun parenthesis-p (char)
  (member char '(#\( #\))))

(defun operator-p (char)
  (member char '(#\* #\+)))

(defun token-p (c)
  (or (digit-char-p c) (parenthesis-p c) (operator-p c)))

(defun next-token (line)
  (position-if #'token-p line))

(defun ->token (char)
  (case char
    (#\( 'open)
    (#\) 'close)
    (#\* '*)
    (#\+ '+)
    (t (- (char-code char) 48))))

(defun tokenize (line)
  (when-it (next-token line)
    (let ((n (1+ it)))
      (cons
       (->token (elt (str:trim (subseq line 0 n)) 0))
       (tokenize (subseq line n))))))

(defun group-exprs (tokens)
  (if-it (position 'open tokens)
         (let* ((groupped (group-exprs (subseq tokens (1+ it))))
                (close-position (position 'close groupped)))
           (append (subseq tokens 0 it)
                   (list (subseq groupped 0 close-position))
                   (subseq groupped (1+ close-position))))
         tokens))

(defun compute (expr)
  (if (numberp expr)
      expr
      (let ((sum (compute (car expr))))
        (loop for (op b) on (cdr expr) by #'cddr do
          (case op
            (+ (incf sum (compute b)))
            (* (setf sum (* sum (compute b))))))
        sum)))

(compute '(1 + 2 * 3 + 4 * 5 + 6)) ;; 71

(defun solve (input)
  (loop for line in (str:lines input)
        sum (compute (group-exprs (tokenize line)))))

(solve (aoc:puzzle 18 2020))

(defun compute2 (expr)
  (cond
    ((numberp expr) expr)
    (t
     (let ((sum (list (compute2 (car expr)))))
       (loop for (op b) on (cdr expr) by #'cddr do
         (case op
           (+ (incf (car sum) (compute2 b)))
           (* (push (compute2 b) sum))))
       (reduce #'* sum)))))

(defun solve2 (input)
  (loop for line in (str:lines input)
        sum (compute2 (group-exprs (tokenize line)))))

(compute2 '(1 + 2 * 3 + 4 * 5 + 6)) ;; 231

(solve2 (aoc:puzzle 18 2020))
