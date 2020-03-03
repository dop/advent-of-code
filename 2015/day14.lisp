(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201514
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201514)

(defparameter *example*
  '((Comet 14 10 127)
    (Dancer 16 11 162)))

(defun get-cycle-distance-and-time (speed travel-time rest-time)
  (list (* travel-time speed) (+ travel-time rest-time)))

(defun final-position (speed travel-time cycle-distance cycle-time time)
  (multiple-value-bind (cycles rest) (floor time cycle-time)
    (+ (* speed (min travel-time rest)) (* cycles cycle-distance))))

(defun winning-position (input final-time)
  (loop :for (name speed travel-time rest-time) :in input
        :maximize (destructuring-bind
                      (cycle-distance cycle-time)
                      (get-cycle-distance-and-time speed travel-time rest-time)
                    (final-position speed travel-time cycle-distance cycle-time final-time))))

(defparameter *input*
  '((Vixen 8 8 53)
    (Blitzen 13 4 49)
    (Rudolph 20 7 132)
    (Cupid 12 4 43)
    (Donner 9 5 38)
    (Dasher 10 4 37)
    (Comet 3 37 76)
    (Prancer 9 12 97)
    (Dancer 37 1 36)))

(winning-position *input* 2503) ;; 2655

(defun winners (input at-time)
  (let ((winners (sort
                  (loop
                    :for (name speed travel-time rest-time) :in input
                    :collect (cons name (destructuring-bind
                                            (cycle-distance cycle-time)
                                            (get-cycle-distance-and-time speed travel-time rest-time)
                                          (final-position speed travel-time cycle-distance cycle-time at-time))))
                  #'> :key #'cdr)))
    (mapcar #'car (car (group-by winners :key #'cdr)))))

(defun group-by (list &key (test #'eq) (key #'identity))
  (when list
    (loop
      :with groups := (list '())
      :for (a b) :on list :do
         (push a (car groups))
         (unless (or (not b) (funcall test (funcall key a) (funcall key b)))
           (push '() groups))
      :finally
         (return (nreverse groups)))))

(defun final-winners (input race-duration)
  (loop
    :with points := (make-hash-table)
    :for i :from 1 :to race-duration
    :do
       (loop :for name :in (winners input i)
             :do (rutils:getsethash name points 0)
                 (incf (gethash name points)))
    :finally
       (return points)))

(sort (rutils:ht->pairs (final-winners *input* 2503))
      #'> :key #'second)
