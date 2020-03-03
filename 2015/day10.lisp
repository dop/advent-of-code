(ql:quickload '(:alexandria :rutils :str))

(defpackage #:aoc201510
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:if-it #:getsethash))

(in-package #:aoc201510)

(defun group-by (fn list)
  (when list
    (loop
      :with groups := (list '())
      :for (a b) :on list :do
         (push a (car groups))
         (unless (or (not b) (funcall fn a b))
           (push '() groups))
      :finally
         (return (nreverse groups)))))

(defun rle (list)
  (mapcar (lambda (group) (list (car group) (length group)))
          (group-by #'eq list)))

(defun digits (number)
  (loop
    :with list
    :while (plusp number)
    :do
       (push (mod number 10) list)
       (setf number (round number 10))
    :finally
       (return list)))

(defun look-and-say (digits)
  (loop :for (number count) :in (rle digits) :collect count :collect number))

(loop
  :with answer := (digits 1113122113)
  :for i :from 0 :below 50 :do
     (setf answer (look-and-say answer))
  :finally
     (return (length answer)))
