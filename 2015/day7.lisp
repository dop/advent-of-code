(ql:quickload '(:alexandria :rutils))

(defpackage #:aoc201507
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:if-it #:getsethash))

(in-package #:aoc201507)

(defparameter *example*
  "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> t
")

(defun parse (input)
  (with-input-from-string (*standard-input* input)
    (loop
      :with circut := (make-hash-table)
      :for t1 := (read *standard-input* nil)
      :for t2 := (read *standard-input* nil)
      :while t1
      :do
         (cond
           ((eq t1 'not)
            (let ((-> (read))
                  (output (read)))
              (declare (ignore ->))
              (setf (gethash output circut) `(not ,t2))))
           (t
            (case t2
              (->
               (let ((output (read)))
                 (setf (gethash output circut) `(value ,t1))))
              (t
               (let ((x (read))
                     (-> (read))
                     (output (read)))
                 (declare (ignore ->))
                 (setf (gethash output circut) (list t2 t1 x)))))))
      :finally
         (return circut))))

(defun run (circut &optional (values (make-hash-table)))
  (loop
    :for output :being :the :hash-keys :of circut
    :do
       (setf (gethash output values)
             (solve (gethash output circut) values circut))
    :finally
       (return values)))

(defun solve (expr values circut)
  (labels ((compute (x)
             (if (symbolp x)
                 (getsethash x values (solve (gethash x circut) values circut))
                 x)))
    (logand #xFFFF
            (ecase (first expr)
              (value
               (compute (second expr)))
              (not
               (lognot (compute (second expr))))
              (and
               (logand (compute (second expr))
                       (compute (third expr))))
              (or
               (logior (compute (second expr))
                       (compute (third expr))))
              (lshift
               (ash (compute (second expr)) (third expr)))
              (rshift
               (ash (compute (second expr)) (- (third expr))))))))
