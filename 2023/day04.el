;; day 4 part 1
(with-current-buffer (find-file-noselect "~/tmp/day4.txt")
  (let* ((lines (string-lines (buffer-string))))
    (loop for game in lines
          for points = (let* ((numbers (string-split game " +"))
                              (pos (seq-position numbers "|"))
                              (winning (mapcar #'string-to-number (seq-subseq numbers (1+ pos))))
                              (guesses (mapcar #'string-to-number (seq-subseq numbers 2 pos))))
                         (loop for guess in guesses count (-contains-p winning guess)))
          when (plusp points)
          sum (expt 2 (1- points))))) ;; 21821

;; day 4 part 2
(with-current-buffer (find-file-noselect "~/tmp/day4.txt")
  (labels ((ht-incf (ht k &optional n)
                    (let ((cnt (gethash k ht 0)))
                      (setf (gethash k ht) (+ cnt (or n 1))))))
    (let* ((counts (make-hash-table))
           (lines (string-lines (buffer-string))))
      (loop for i from 1 to (length lines) do (ht-incf counts i))
      (loop for i from 1
            for game in lines
            for points = (let* ((numbers (string-split game " +"))
                                (pos (seq-position numbers "|"))
                                (winning (mapcar #'string-to-number (seq-subseq numbers (1+ pos))))
                                (guesses (mapcar #'string-to-number (seq-subseq numbers 2 pos))))
                           (loop for guess in guesses count (-contains-p winning guess)))
            do (loop for j from 1 to points do
                     (ht-incf counts (+ i j) (gethash i counts))))
      (apply #'+ (hash-table-values counts))))) ;; 5539496
