;; -*- lexical-binding: t; -*-

(load (expand-file-name "aoc.el" default-directory))
(load (expand-file-name "heap.el" default-directory))
(load (car (file-expand-wildcards
            (expand-file-name "elpa/queue-*/queue.el" user-emacs-directory))))

;; day 22 part 1 & 2

(with-puzzle "day22.txt"
  (cl-labels ((brick> (a b)
                (> (elt a 2) (elt b 2)))
              (brick< (a b)
                (< (elt a 2) (elt b 2)))
              (overlapxy? (a b)
                (pcase-exhaustive (list a b)
                  (`([,ax ,ay ,_ ,axx ,ayy ,_] [,bx ,by ,_ ,bxx ,byy ,_])
                   (not (or (< axx bx) (< bxx ax) (< byy ay) (< ayy by))))))
              (overlapz? (a b)
                (pcase-exhaustive (list a b)
                  (`([,_ ,_ ,az ,_ ,_ ,azz] [,_ ,_ ,bz ,_ ,_ ,bzz])
                   (not (or (< azz bz) (< bzz az))))))
              (distance (a b)
                (1- (- (elt b 2) (elt a 5)))))
    (let* ((numbers (mapcar #'string-to-number
                            (string-split (buffer-string) "[~,\n]" t)))
           (bricks (seq-into (seq-partition (apply #'vector numbers) 6) 'vector))
           (sorted (seq-into (seq-sort #'brick< bricks) 'vector)))

      ;; drop bricks
      (loop for i from 0 below (length sorted) do
            (let* ((a (elt sorted i)))
              ;; (pri i)
              (loop with d = 1e100
                    for j from (1- i) downto 0
                    for b = (elt sorted j)
                    when (overlapxy? a b)
                    do (setf d (min d (distance b a)))
                    finally
                    (let ((lim (1- (elt a 2))))
                      (decf (elt a 2) (min lim d))
                      (decf (elt a 5) (min lim d))))))

      (cl-labels ((supports? (a b)
                    (and (= (1+ (elt a 5)) (elt b 2))
                         (overlapxy? a b))))
        (let ((supporting (make-hash-table))
              (uniques (make-hash-table))
              (supported-by (make-hash-table)))

          ;; brick supportees & supporters
          (loop for i from 0 below (length bricks)
                for a = (elt bricks i)
                do (setf (gethash a uniques) nil)
                do (loop for j from 0 below (length bricks)
                         for b = (elt bricks j)
                         unless (eq i j)
                         do (when (supports? a b)
                              (push a (gethash b supported-by))
                              (push b (gethash a supporting))
                              (push b (gethash a uniques)))))

          ;; remove shared supported bricks
          (loop for i from 0 below (length bricks)
                for a = (elt bricks i)
                do (loop for j from 0 below (length bricks)
                         for b = (elt bricks j)
                         unless (or (eq i j) (not (eq (elt a 5) (elt b 5))))
                         do (let ((as (gethash a uniques))
                                  (bs (gethash b supporting)))
                              (setf (gethash a uniques)
                                    (seq-difference as bs #'eq)))))

          ;; how many have no unique supported bricks?
          (pr "part 1: %d" (seq-count #'null (hash-table-values uniques))) ;; 386

          (loop with sum = 0
                for i from 0 below (length sorted)
                for a = (elt sorted i)
                do (let ((counts (make-hash-table))
                         (Q (make-queue)))
                     ;; start with uniquely supported bricks
                     (loop for u in (gethash a uniques) do (queue-enqueue Q u))

                     (loop until (queue-empty Q)
                           for b = (queue-dequeue Q)
                           do (counthash b counts)
                           do (loop for u in (gethash b supporting)
                                    ;; enque only if u has no stationary supporting bricks
                                    unless (seq-difference (gethash u supported-by)
                                                           (hash-table-keys counts))
                                    do (queue-enqueue Q u))
                           finally
                           (incf sum (hash-table-count counts))))
                finally
                (pr "part 2: %d" sum))))))) ;; 39933
