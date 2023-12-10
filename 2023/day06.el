(require 'cl)
(require 'dash)



;; day 6 part 1
(with-current-buffer (find-file-noselect "~/tmp/day6.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " +"))))
    (pcase-exhaustive (string-split (buffer-string) "[\n:]" t " +")
      (`(,_ ,times ,_ ,records)
       (apply #'* (-zip-with (lambda (time record)
                               (loop for i from 1 below time
                                     count (< record (* i (- time i)))))
                             (numbers times)
                             (numbers records))))))) ;; 800280



;; day 6 part 2

(with-current-buffer (find-file-noselect "~/tmp/day6.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " +"))))
    (pcase-exhaustive (string-split (buffer-string) "[\n:]" t " +")
      (`(,_ ,times ,_ ,records)
       (let ((time (number (string-replace " " "" times)))
             (record (number (string-replace " " "" records))))
         (loop for i from 1 below time
               count (< record (* i (- time i))))))))) ;; 45128024
