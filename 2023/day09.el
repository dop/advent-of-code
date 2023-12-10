(require 'cl)
(require 'subr-x)



;; day 9 part 1

(with-current-buffer (find-file-noselect "~/tmp/day9.txt")
  (let ((seqs
         (mapcar (lambda (line) (mapcar #'string-to-number (string-split line "\s+")))
                 (string-lines (buffer-string)))))
    (loop for seq in seqs
          sum (loop with diffs = seq and lasts = (last seq)
                    until (apply #'= diffs)
                    do (setf diffs (loop for (a b) on diffs when (and a b) collect (- b a)))
                    do (push (last1 diffs) lasts)
                    finally (return (apply #'+ lasts)))))) ;; 1798691765



;; day 9 part 2

(with-current-buffer (find-file-noselect "~/tmp/day9.txt")
  (let ((seqs
         (mapcar (lambda (line) (mapcar #'string-to-number (string-split line "\s+")))
                 (string-lines (buffer-string)))))
    (loop for seq in seqs
          sum (loop with diffs = seq and heads = (list (car seq))
                    until (apply #'= diffs)
                    do (setf diffs (loop for (a b) on diffs when (and a b) collect (- b a)))
                    do (push (car diffs) heads)
                    finally (return (seq-reduce (lambda (a b) (- b a))
                                                (cdr heads) (car heads))))))) ;; 1104

