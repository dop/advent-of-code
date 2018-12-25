(ql:quickload '(:rutils :optima))
(cl:defpackage #:aoc-2018-day14 (:use #:cl #:optima))
(cl:in-package #:aoc-2018-day14)

(let ((recipes (make-array (+ 11 637061) :fill-pointer 0)))
  (vector-push 3 recipes)
  (vector-push 7 recipes)
  (loop
    :for a := 0 :then (rem (+ a (1+ (aref recipes a))) (length recipes))
    :for b := 1 :then (rem (+ b (1+ (aref recipes b))) (length recipes))
    :until (>= (length recipes) (+ 10 637061)) :do
      (let* ((sum (+ (aref recipes a) (aref recipes b))))
        (multiple-value-bind (k l) (truncate sum 10)
          (when (plusp k) (vector-push k recipes))
          (vector-push l recipes)))
    :finally
       (return (coerce
                (map 'list
                     (lambda (n) (code-char (+ 48 n)))
                     (subseq recipes 637061 (+ 10 637061)))
                'string))))

;; => "3138510102"

(defun find-pattern (pattern)
  (let ((recipes (make-array 1000000000 :fill-pointer 0))
        (len (length pattern)))
    (vector-push 3 recipes)
    (vector-push 7 recipes)
    (loop
      :for a := 0 :then (rem (+ a (1+ (aref recipes a))) (length recipes))
      :for b := 1 :then (rem (+ b (1+ (aref recipes b))) (length recipes))
      :for i :from (- len)
      :until (and (plusp i)
                  (equalp (subseq recipes i (min (+ i len) (length recipes)))
                          pattern))
      :do
         (let* ((sum (+ (aref recipes a) (aref recipes b))))
           (multiple-value-bind (k l) (truncate sum 10)
             (when (plusp k) (vector-push k recipes))
             (vector-push l recipes)))
      :finally
         (return i))))

(find-pattern #(6 3 7 0 6 1)) ;; => 20179081

(time (find-pattern #(6 3 7 0 6 1)))

