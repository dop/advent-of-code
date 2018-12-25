(ql:quickload '(:rutils :dlist))
(cl:defpackage #:aoc-2018-day9 (:use #:cl))
(cl:in-package #:aoc-2018-day9)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *print-circle* t))

;; This works a lot faster with higher GC limits.
#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 1024 1024 1024))
#+clozure (ccl:set-lisp-heap-gc-threshold (* 1024 1024 1024))

;; 418 players; last marble is worth 71339 points

(defparameter *cursor* (dlist:dcons nil 0 nil))

;; putting in progn, to return nil, because swank chokes on circular structure
;; trying to pretty print.
(progn
  (setf (dlist:next *cursor*) *cursor*)
  (setf (dlist:prev *cursor*) *cursor*)
  nil)



(defparameter *marble-count* 7133900)
(defparameter *elf-count* 418)
(defparameter *elf-scores* (make-hash-table :test 'eq))

(time
 (loop :for i :from 1 :to *marble-count* :do
   (let ((elf (1+ (rem (1- i) *elf-count*))))
     (if (zerop (rem i 23))
         (progn
           (dotimes (step 7)
             (setf *cursor* (dlist:prev *cursor*)))
           (incf (gethash elf *elf-scores* 0)
                 (+ i (dlist:data *cursor*)))
           (let ((prev (dlist:prev *cursor*))
                 (next (dlist:next *cursor*)))
             (setf (dlist:next prev) next)
             (setf (dlist:prev next) prev)
             (setf *cursor* next)))
         (let* ((marble1 (dlist:nthdcons 1 *cursor*))
                (marble2 (dlist:nthdcons 2 *cursor*))
                (new     (dlist:dcons marble1 i marble2)))
           (setf *cursor* new)
           (setf (dlist:next marble1) *cursor*)
           (setf (dlist:prev marble2) *cursor*))))))

(car
 (sort (rutils:hash-table-to-alist *elf-scores*)
       #'> :key #'cdr)) ;; => (161 . 3482394794)
