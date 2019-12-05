(in-package :advent-of-code-2019-day4)

;; 357253 - 892942

(defun satisfactory-p (number)
  (loop :with p :and two-adjacent :and non-decreasing := t
        :for x := number :then (floor x 10)
        :for n := (rem x 10)
        :while (plusp x)
        :do (setf two-adjacent (or two-adjacent (and p (= n p)))
                  non-decreasing (and non-decreasing (or (null p) (>= p n)))
                  p n)
        :finally
           (return (and two-adjacent non-decreasing))))

(loop :for num :from 357253 :to 892942 :count (satisfactory-p num)) ; => 530 (10 bits, #x212)

(defun fixed-satisfactory-p (number)
  (loop :with p :and same-count := 0 :and two-adjacent :and non-decreasing := t
        :for x := number :then (floor x 10)
        :for n := (rem x 10)
        :while (plusp x)
        :do (setf non-decreasing (and non-decreasing (or (null p) (>= p n)))
                  two-adjacent (or two-adjacent (and p (/= n p) (= 1 same-count)))
                  same-count (if (and p (= n p)) (1+ same-count) 0)
                  p n)
        :finally
           (return (and (or two-adjacent (= 1 same-count)) non-decreasing))))

(loop :for num :from 357253 :to 892942 :count (fixed-satisfactory-p num)) ; => 324 (9 bits, #x144)
