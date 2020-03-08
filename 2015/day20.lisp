(defun factors (n)
  (loop with factors
        for i from 2 below (sqrt (1+ n))
        do (when (= 0 (rem n i))
             (push i factors)
             (unless (= n (* i i))
               (push (/ n i) factors)))
        finally (return (cons 1 (cons n factors)))))

(loop for i from 2000 to 2900000 do
  (when (<= 2900000 (apply #'+ (factors i))) (return i))) ;; 665280
