(defpackage #:day08
  (:use #:cl)
  (:import-from #:dop #:fn #:kw #:with-collecting #:counting #:collecting)
  (:import-from #:trivia #:match #:vector* #:ematch)
  (:import-from #:rutils #:->> #:% #:xor #:it #:=> #:when-it)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day08)

(defparameter +input+
  (->> (aoc:puzzle 8 2020)
       (str:lines)
       (mapcar #`(str:split " " %))
       (coerce % 'vector)))

(defparameter +example+
  (->> "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"
              (str:lines)
       (mapcar #`(str:split " " %))
       (coerce % 'vector)))

(let ((acc 0)
      (visited #h())
      (ip 0))
  (loop
    (destructuring-bind (cmd arg) (elt +input+ ip)
      ;; (format t "~4d ~a ~a~%" ip cmd arg)
      (when (nth-value 1 (gethash ip visited))
        (return acc))
      (setf (gethash ip visited) t)
      (ematch cmd
        ("nop"
         (incf ip))
        ("jmp"
         (incf ip (parse-integer arg)))
        ("acc"
         (incf acc (parse-integer arg))
         (incf ip)))))) ;; 1262


(let ((input +input+))
  (let ((options (loop for i from 0
                       for (cmd arg) across input
                       if (member cmd '("nop" "jmp") :test #'equal)
                         collect i))
        (target (length input)))
    (loop named main for option in options do
      (let ((acc 0)
            (ip 0)
            (visited #h()))
        (symbol-macrolet ((c (car (elt input option))))
          (if (string= "nop" c) (setf c "jmp") (setf c "nop")))
        (loop named inner do
          (when (= ip target)
            (return-from main acc))
          (destructuring-bind (cmd arg) (elt input ip)
            ;; (format t "~4d ~a ~a~%" ip cmd arg)
            (when (nth-value 1 (gethash ip visited))
              (symbol-macrolet ((c (car (elt input option))))
                (if (string= "nop" c) (setf c "jmp") (setf c "nop")))
              (return-from inner))
            (setf (gethash ip visited) t)
            (ematch cmd
              ("nop"
               (incf ip))
              ("jmp"
               (incf ip (parse-integer arg)))
              ("acc"
               (incf acc (parse-integer arg))
               (incf ip))))))))) ;; 1643
