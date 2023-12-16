(load "~/Projects/advent-of-code/2023/aoc.el")



;; day 16 part 1

(defun day15hash (string)
  (let ((rez 0))
    (loop for c across string
          do (incf rez c)
          do (setf rez (% (* 17 rez) 256)))
    rez))

(with-puzzle "day15.txt"
  (seq-reduce (lambda (sum step) (+ sum (day15hash step)))
              (string-split (buffer-string) "[,\n]" t "\n")
              0)) ;; 510388

;; day 15 part 2

(defun parse-step (step)
  (let ((len (length step)))
    (if (eq ?- (elt step (1- len)))
        (list :rem (subseq step 0 (1- len)))
      (list :set (subseq step 0 (- len 2)) (- (elt step (1- len)) ?0)))))

(with-puzzle "day15.txt"
  (let ((boxes (make-vector 256 nil))
        (steps (string-split (buffer-string) "[,\n]" t "\n")))
    ;; (loop for i from 0 below 256 do (setf (aref boxes i) ()))
    (loop for step in steps
          ;; do (pr step)
          do (pcase (parse-step step)
               (`(:set ,label ,fl)
                ;; (pr "set %s %d" label fl)
                (let* ((i (day15hash label))
                       (j (loop for k from 0
                                for (l . _) in (aref boxes i)
                                when (equal label l)
                                do (return k))))
                  (let ((lenses (aref boxes i)))
                    (setf (aref boxes i)
                          (if j (append (subseq lenses 0 j)
                                        (list (cons label fl))
                                        (subseq lenses (1+ j)))
                            (append lenses (list (cons label fl))))))))
               (`(:rem ,label)
                ;; (pr "remove %s" label)
                (let ((i (day15hash label)))
                  (setf (aref boxes i)
                        (seq-remove (lambda (lens) (equal label (car lens)))
                                    (aref boxes i))))))
          ;; do (loop for i from 0 below 256
          ;;          do (when-let ((lenses (aref boxes i)))
          ;;               (pr "%d %s" i lenses))
          ;;          finally (pr ""))
          )

    (loop for i from 0 below 256
          sum (if-let ((lenses (aref boxes i)))
                  (loop for j from 1
                        for (_ . fl) in lenses
                        sum (* (1+ i) j fl))
                0)))) ;; 291774
