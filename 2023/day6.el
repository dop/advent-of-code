(require 'cl)
(require 'dash)



;; day 6 part 1
(with-current-buffer (find-file-noselect "~/tmp/day6.example.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " +"))))
    (pcase-exhaustive (string-split (buffer-string) "[\n:]" t " +")
      (`(,_ ,times ,_ ,records)
       (-zip-with (lambda (time record)
                    (list time record))
                  (numbers times)
                  (numbers records))))))



;; day 6 part 2

