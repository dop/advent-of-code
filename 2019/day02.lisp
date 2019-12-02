(in-package :advent-of-code-2019-day2)

(defparameter *input*
  (mapcar #'parse-integer (str:split "," (aocu:get-input-for-day 2))))

(defparameter *code* (aocu:array-of-list *input*))

(defmacro @@ (i)
  `(aref *code* (aref *code* ,i)))

(defun op-1 (i j k)
  (setf (@@ k) (+ (@@ i) (@@ j))))

(defun op-2 (i j k)
  (setf (@@ k) (* (@@ i) (@@ j))))

(defun execute (&optional (*code* *code*))
  (loop :for i := 0 :then (incf i 4) :do
    (ecase (aref *code* i)
      (99
       (return *code*))
      (1
       (op-1 (+ i 1) (+ i 2) (+ i 3)))
      (2
       (op-2 (+ i 1) (+ i 2) (+ i 3))))))

;; Part 1
(progn
  (setf *code* (aocu:array-of-list *input*)
        (aref *code* 1) 12
        (aref *code* 2) 2)
  (aref (execute *code*) 0)) ; => 5098658 (23 bits, #x4DCCA2)

;; Part 2
(loop :named outer :for noun :from 0 :to 99 :do
  (loop :for verb :from 9 :to 99 :do
    (setf *code* (aocu:array-of-list *input*)
          (aref *code* 1) noun
          (aref *code* 2) verb)
    (execute)
    (when (= 19690720 (aref *code* 0))
      (return-from outer (+ verb (* 100 noun)))))) ; => 5064 (13 bits, #x13C8)
