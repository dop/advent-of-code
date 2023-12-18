;; -*- lexical-binding: t; -*-

(load "~/Projects/advent-of-code/2023/aoc.el")
(load "~/Projects/advent-of-code/2023/heap.el")



;; day 18 part 1

(with-puzzle (:in "day18.txt")
  (let ((pos (list 0 0))                              ;; r c
        (bl 1e100) (br -1e100) (bu 1e100) (bd -1e100) ;; bounds
        (ground (make-hash-table :test 'equal)))
    (cl-labels ((ref (r c)
                  (gethash (list r c) ground)))
      (loop for line in (string-lines (buffer-string))
            for (a b color) = (string-split line " ")
            do (let* ((dir (case (elt a 0)
                             (?R (list 0 1))
                             (?L (list 0 -1))
                             (?U (list -1 0))
                             (?D (list 1 0))))
                      (dist (string-to-number b)))
                 (loop for i from 0 below dist
                       do (setf pos (map 'list #'+ pos dir))
                       do (setf (gethash pos ground) t)
                       do (setf bl (min bl (cadr pos))
                                br (max br (cadr pos))
                                bu (min bu (car pos))
                                bd (max bd (car pos))))
                 ))
      (loop with sum = 0 and inside = nil
            for r from bu to bd
            do (loop for c from bl to br
                     do (cond ((ref r c)
                               (princ "#")
                               (incf sum)
                               (cond (inside
                                      (setf inside
                                            (not (or (and (ref (1- r) c) (ref (1+ r) c)) ;; |
                                                     (and (not (ref (1- r) c)) (ref (1+ r) c)) ;; 7
                                                     ))))
                                     (t
                                      (setf inside
                                            (or (and (ref (1- r) c) (ref (1+ r) c)) ;; |
                                                (and (not (ref (1- r) c)) (ref (1+ r) c)) ;; F
                                                )))))
                              (t
                               (when inside (incf sum))
                               (princ "."))))
            do (terpri)
            finally (return sum)))))


;; day 18 part 2

(with-puzzle (:in "day18.txt")
  (let ((points (list (list 0 0)))
        (perimeter 0)
        (ground (make-hash-table :test 'equal)))
    (cl-labels ((zip (fn a b)
                  (map 'list fn a b)))
      (loop for line in (string-lines (buffer-string))
            for (_ _ color) = (string-split line "[ ()]" t)
            do (let ((dir (case (elt color 6)
                            (?0 (list 0 1))
                            (?1 (list 1 0))
                            (?2 (list 0 -1))
                            (?3 (list -1 0))))
                     (dist (string-to-number (subseq color 1 6) 16)))
                 (pr "%s %s %s" color dir dist)
                 (incf perimeter dist)
                 (push (map 'list '+ (car points) (map 'list #'+ (map 'list #'* dir (list dist dist))))
                       points)

                 ;; (setf bl (min bl (cadr pos))
                 ;;       br (max br (cadr pos))
                 ;;       bu (min bu (car pos))
                 ;;       bd (max bd (car pos)))
                 )))
    (+ (/ perimeter 2)
       (/ (loop for ((a b) (c d)) on points
                when (and c d)
                sum (- (* a d) (* b c)))
          2)
       1))) ;; 194033958221830


