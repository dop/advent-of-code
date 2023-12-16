(load "~/Projects/advent-of-code/2023/aoc.el")



;; day 16 part 1

(defun move-beam (beam)
  (cl-destructuring-bind (r c dir) beam
    (case dir
      (R (list r (1+ c) dir))
      (L (list r (1- c) dir))
      (U (list (1- r) c dir))
      (D (list (1+ r) c dir)))))

(defun count-energized-tiles (beam rows cols block)
  (cl-macrolet ((ref (r c)
                  `(aref block (+ ,c (* ,r cols)))))
    (cl-labels ((valid-beam-p (beam)
                  (cl-destructuring-bind (r c _) beam
                    (and (< -1 r rows) (< -1 c cols)))))

      ;; (print-grid rows cols block)
      ;; (pr "")

      (let ((seen (make-hash-table :test 'equal))
            (beams (list beam)))
        (while beams
          ;; (pr "beams %s" beams)
          (loop with next
                for beam in beams
                for (r c dir) = beam
                ;; do (pr "  moving %s valid:%s seen:%s"
                ;;        beam
                ;;        (valid-beam-p beam)
                ;;        (not (not (gethash beam seen))))
                when (and (valid-beam-p beam)
                          (not (gethash beam seen)))
                do (progn
                     (set-add seen beam)
                     ;; (pr "  looking at %c" (ref r c))
                     (case (ref r c)
                       (?. (push beam next))
                       (?| (case dir
                             ((L R)
                              (push (list r c 'U) next)
                              (push (list r c 'D) next))
                             (t
                              (push beam next))))
                       (?- (case dir
                             ((U D)
                              (push (list r c 'L) next)
                              (push (list r c 'R) next))
                             (t
                              (push beam next))))
                       (?\\ (ecase dir
                              (R (push (list r c 'D) next))
                              (L (push (list r c 'U) next))
                              (U (push (list r c 'L) next))
                              (D (push (list r c 'R) next))))
                       (?/ (ecase dir
                             (R (push (list r c 'U) next))
                             (L (push (list r c 'D) next))
                             (U (push (list r c 'R) next))
                             (D (push (list r c 'L) next))))))
                do (setf beams (mapcar #'move-beam next))
                ;; do (pr "  next %s" beams)
                ))

        (let ((count (make-hash-table)))
          (loop for (r c _) in (hash-table-keys seen)
                do (counthash (+ (* r cols) c) count)
                finally (progn
                          ;; (pr "%s" (hash-table-alist count))
                          (return (hash-table-count count)))))))))

(with-puzzle "day16.txt"
  (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
    (count-energized-tiles (list 0 0 'R) rows cols block))) ;; 6795

;; day 16 part 2

(with-puzzle "day16.txt"
  (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
    (max
     (loop for r from 0 below rows
           maximize (count-energized-tiles (list r 0 'R) rows cols block))
     (loop for r from 0 below rows
           maximize (count-energized-tiles (list r (1- cols) 'L) rows cols block))
     (loop for c from 0 below cols
           maximize (count-energized-tiles (list 0 c 'D) rows cols block))
     (loop for c from 0 below cols
           maximize (count-energized-tiles (list (1- rows) c 'U) rows cols block))))) ;; 7154
