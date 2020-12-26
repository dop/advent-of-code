(defpackage #:day19
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:map-product))

(in-package #:day19)

(defun parse-rule (line)
  (destructuring-bind (id . rules) (cl-ppcre:split #?"[:|]" line)
    (cons (parse-integer id)
          (mapcar #'(lambda (rule)
                      (mapcar (lambda (rule-part)
                                (if (every #'digit-char-p rule-part)
                                    (parse-integer rule-part)
                                    (elt rule-part 1)))
                              (str:split " " (str:trim rule) :omit-nulls t)))
                  rules))))

(defun take-while (pred seq)
  (do ((i 0 (1+ i)))
      ((or (= i (length seq))
           (not (funcall pred (elt seq i))))
       (values (subseq seq 0 i) (subseq seq i)))))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defun scan (preds seq)
  (let ((rez))
    (loop for pred in preds do
      (multiple-value-bind (match rest) (take-while pred (nth-value 1 (take-until pred seq)))
        (format t "~a ~a~%" match rest)
        (push match rez)
        (setf seq rest)))
    (nreverse rez)))

(scan (list #'digit-char-p #'digit-char-p #'digit-char-p) "1: 1 2")

(defparameter *rules* nil)

(defun match-part (part sequence)
  (when (plusp (length sequence))
    (etypecase part
      (number
       (match-rule part sequence))
      (character
       (and (char= part (elt sequence 0))
            (list (subseq sequence 1)))))))

(defun match-parts (parts sequence)
  (reduce (lambda (seqs part)
            (flat-map #`(match-part part %) seqs))
          parts
          :initial-value (list sequence)))

(defun generate-options (id option &optional (limit 10))
  (let ((options (list option)))
    (when (member id option)
      (let ((options (list option)))
        (loop repeat limit do
          (push (loop for o in (car options)
                      if (eq o id)
                        append option
                      else
                        collect o)
                options))))
    options))

(defun match-rule (id sequence)
  (declare (type number id))
  (let ((options (gethash id *rules*))
        (results))
    (loop for option in options do
      (loop for o in (generate-options id option) do
        (when-it (match-parts o sequence)
          (setf results (append results it)))))
    results))

(defmacro while-it (expr &body body)
  `(loop for it = ,expr while it do ,@body))

(defun parse-input (input)
  (let ((rules #h()) (messages))
    (with-input-from-string (in input)
      (while-it (read-line in nil nil)
        (when (plusp (length it))
          (if (digit-char-p (elt it 0))
              (destructuring-bind (id . options) (parse-rule it)
                (setf (gethash id rules) options))
              (push it messages))))
      (list rules messages))))

(defun solve1 (input)
  (destructuring-bind (*rules* messages) (parse-input input)
    (length (remove-if-not #`(some #`(string= "" %) (match-rule 0 %))
                           messages))))

(defun solve2 (input)
  (destructuring-bind (*rules* messages) (parse-input input)
    (setf (gethash 8 *rules*) '((42 8) (42))
          (gethash 11 *rules*) '((42 11 31) (42 31)))
    (length (remove-if-not #`(some #`(string= "" %) (match-rule 0 %))
                           messages))))

(values
 (solve1 (aoc:puzzle))
 (solve2 (aoc:puzzle)))
