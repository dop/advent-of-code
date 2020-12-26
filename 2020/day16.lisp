(defpackage #:day16
  (:use #:cl #:rutils))

(in-package #:day16)

(defparameter *example*
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
")

(defun parse-rule (line)
  (list
   (subseq line 0 (position #\: line))
   (mapcar #'parse-integer
           (str:split "-"
                      (subseq line (position-if #'digit-char-p line) (search " or " line))))
   (mapcar #'parse-integer
           (str:split "-"
                      (subseq line (+ 4 (search " or " line)))))))

(defun solve (input)
  (destructuring-bind (rules ticket nearby-tickets) (cl-ppcre:split #?"\n\n" input)
    (let ((field-rules (mapcar #'parse-rule (str:lines rules)))
          (tickets (mapcar (lambda (line)
                             (mapcar #'parse-integer (str:split "," line)))
                           (nthcdr 1 (str:lines nearby-tickets))))
          (sum 0))
      ;; (format t "~a~%" tickets)
      (loop for ticket in tickets do
        (loop for number in ticket do
          ;; (format t "number ~a~%" number)
          (let ((bad t))
            (loop for (name (a b) (c d)) in field-rules do
              ;; (format t "rule ~a ~a-~a ~a-~a~%" name a b c d)
              (when (or (<= a number b) (<= c number d))
                (setf bad nil)))
            (when bad
              (incf sum number)))))
      sum)))

(solve *example*) ;; 71

(solve (aoc:puzzle 16 2020)) ;; 27870

(defun valid-number-p (rules number)
  (loop for (name (a b) (c d)) in rules
        when (or (<= a number b) (<= c number d))
          collect name))

(defun valid-ticket-p (rules ticket)
  (every (lambda (number) (valid-number-p rules number)) ticket))

(defun solve2 (input)
  (destructuring-bind (rules ticket nearby-tickets) (cl-ppcre:split #?"\n\n" input)
    (let* ((field-rules (mapcar #'parse-rule (str:lines rules)))
           (my-ticket (mapcar #'parse-integer (str:split "," (nth 1 (str:lines ticket)))))
           (tickets (mapcar (lambda (line)
                              (mapcar #'parse-integer (str:split "," line)))
                            (nthcdr 1 (str:lines nearby-tickets))))
           (valid-tickets (remove-if-not (lambda (ticket)
                                           (valid-ticket-p field-rules ticket))
                                         tickets))
           (answer (make-hash-table)))

      ;; (loop for (name a b) in field-rules do
      ;;   (loop for i from 0 below (length field-rules) do
      ;;     (push name (gethash i answer))))

      (loop for i below (length field-rules) do
        (loop for ticket in valid-tickets do
          (let ((number (nth i ticket)))
            (push (valid-number-p field-rules number) (gethash i answer)))))

      (let ((almost (->> (ht->alist answer)
                         (mapcar (lambda (entry)
                                   (reduce #`(intersection % %% :test #'equal) (cdr entry))))
                         (mapindex #`(cons % %%)))))
        (loop for i below 100 do
          (let ((singles (remove-if #`(> (length (cdr %)) 1) almost))
                (uncertain (remove-if-not #`(> (length (cdr %)) 1) almost)))
            (loop for (_ field) in singles do
              (setf uncertain
                    (loop for (id . fields) in uncertain
                          collect (cons id (remove field fields)))))
            (setf almost (append singles uncertain))))

        (->> almost
             (remove-if-not #`(str:starts-with-p "departure" (second %)))
             (mapcar #'car)
             (mapcar #`(nth % my-ticket))
             (reduce #'*))))))

(defparameter *example* "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
")

(solve2 (aoc:puzzle 16 2020))
