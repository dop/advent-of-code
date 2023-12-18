;; -*- lexical-binding: t; -*-

(load "~/Projects/advent-of-code/2023/aoc.el")
(load "~/Projects/advent-of-code/2023/heap.el")



;; day 17 part 1

(with-puzzle (:in "day17.txt" :out t)
  (pcase (read-grid (car (string-split (buffer-string) "\n\n" t "\n")))
    (`(,rows ,cols ,block)
     ;; (print-grid rows cols block)

     (let ((Q (make-heap (lambda (a b) (< (car a) (car b)))))
           (heats (make-hash-table :test 'equal)))
       (cl-macrolet ((ref (r c)
                       `(aref block (+ ,c (* ,r cols)))))
         (cl-labels ((valid-pos-p (r c)
                       (and (< -1 r rows) (< -1 c cols)))
                     (valid-state-p (r c sr sc)
                       (and (valid-pos-p r c)
                            (or (and (= sr 0) (or (< 0 sc 4) (> 0 sc -4)))
                                (and (= sc 0) (or (< 0 sr 4) (> 0 sr -4))))))
                     (next-states (state)
                       (cl-destructuring-bind (r c sr sc) state
                         (seq-filter
                          (partial #'apply #'valid-state-p)
                          (collecting next
                            (when (not (= sr 0))
                              (next (list r (1+ c) 0 1))
                              (next (list r (1- c) 0 -1)))
                            (when (not (= sc 0))
                              (next (list (1+ r) c 1 0))
                              (next (list (1- r) c -1 0)))
                            (when (< 0 sr 3)
                              (next (list (1+ r) c (1+ sr) 0)))
                            (when (> 0 sr -3)
                              (next (list (1- r) c (1- sr) 0)))
                            (when (< 0 sc 3)
                              (next (list r (1+ c) 0 (1+ sc))))
                            (when (> 0 sc -3)
                              (next (list r (1- c) 0 (1- sc))))
                            (when (= r c sr sc 0)
                              (next (list 1 0 1 0))
                              (next (list 0 1 0 1))))))))

           (heap-add Q '(0 0 0 0 0))
           ;; (setf (gethash '(0 0 0 0) heats) 0)

           (while (not (heap-empty Q))
             (cl-destructuring-bind (heat . state) (heap-delete-root Q)
               (unless (gethash state heats)
                 (setf (gethash state heats) heat)
                 ;; (pr "%s" state)
                 ;; (pr "  heat %s" heat)
                 (loop for next in (next-states state)
                       ;; unless (gethash next heats)
                       do (cl-destructuring-bind (r c sr sc) next
                            (heap-add Q (cons (+ heat (- (ref r c) ?0)) next))))

                 ;; (setf Q (sort Q (lambda (a b) (< (car a) (car b)))))
                 )))

           (pr "%d" (hash-table-count heats))

           (apply #'min
                  (mapcar #'cdr
                          (seq-filter (lambda (entry)
                                        (equal (subseq (car entry) 0 2)
                                               (list (1- rows) (1- cols))))
                                      (hash-table-alist heats))))))))))

;; day 17 part 2

(with-puzzle (:in "day17.txt" :out t)
  (pcase (read-grid (car (string-split (buffer-string) "\n\n" t "\n")))
    (`(,rows ,cols ,block)
     ;; (print-grid rows cols block)

     (let ((Q (make-heap (lambda (a b) (< (car a) (car b)))))
           (heats (make-hash-table :test 'equal)))
       (cl-macrolet ((ref (r c)
                       `(aref block (+ ,c (* ,r cols)))))
         (cl-labels ((valid-pos-p (r c)
                       (and (< -1 r rows) (< -1 c cols)))
                     (valid-state-p (r c sr sc)
                       (and (valid-pos-p r c)
                            (or (and (= sr 0) (or (< 0 sc 11) (> 0 sc -11)))
                                (and (= sc 0) (or (< 0 sr 11) (> 0 sr -11))))))
                     (next-states (state)
                       (cl-destructuring-bind (r c sr sc) state
                         (seq-filter
                          (partial #'apply #'valid-state-p)
                          (collecting next
                            (when (and (= sc 0) (or (< 3 sr) (> -3 sr)))
                              (next (list r (1+ c) 0 1))
                              (next (list r (1- c) 0 -1)))
                            (when (and (= sr 0) (or (< 3 sc) (> -3 sc)))
                              (next (list (1+ r) c 1 0))
                              (next (list (1- r) c -1 0)))
                            (when (< 0 sr 11)
                              (next (list (1+ r) c (1+ sr) 0)))
                            (when (> 0 sr -11)
                              (next (list (1- r) c (1- sr) 0)))
                            (when (< 0 sc 11)
                              (next (list r (1+ c) 0 (1+ sc))))
                            (when (> 0 sc -11)
                              (next (list r (1- c) 0 (1- sc))))
                            (when (= r c sr sc 0)
                              (next (list 1 0 1 0))
                              (next (list 0 1 0 1))))))))

           (heap-add Q '(0 0 0 0 0))
           ;; (setf (gethash '(0 0 0 0) heats) 0)

           (while (not (heap-empty Q))
             (cl-destructuring-bind (heat . state) (heap-delete-root Q)
               (unless (gethash state heats)
                 (setf (gethash state heats) heat)
                 ;; (pr "%s" state)
                 ;; (pr "  heat %s" heat)
                 (loop for next in (next-states state)
                       unless (gethash next heats)
                       do (cl-destructuring-bind (r c sr sc) next
                            (heap-add Q (cons (+ heat (- (ref r c) ?0)) next))))

                 ;; (setf Q (sort Q (lambda (a b) (< (car a) (car b)))))
                 ))

             ;; (pr "count: %d, max: %s"
             ;;     (hash-table-count heats)
             ;;     (seq-reduce (pcase-lambda (`(,mr . ,mc) `(,r ,c ,_ ,_))
             ;;                   (cons (max mr r) (max mc c)))
             ;;                 (mapcar #'car (hash-table-alist heats))
             ;;                 (cons 0 0)))
             ;; (when-let
             ;;     ((bests (seq-filter (lambda (entry)
             ;;                           (cl-destructuring-bind (r c sr sc) (car entry)
             ;;                             (and (or (and (= sc 0) (< 3 sr 11))
             ;;                                      (and (= sr 0) (< 3 sc 11)))
             ;;                                  (= c (1- cols))
             ;;                                  (= r (1- rows)))))
             ;;                         (hash-table-alist heats))))
             ;;   (pr "best: %d" (apply #'min (mapcar #'cdr bests))))
             )

           (pr "%d" (hash-table-count heats))

           (apply #'min
                  (mapcar #'cdr
                          (seq-filter (lambda (entry)
                                        (cl-destructuring-bind (r c sr sc) (car entry)
                                          (and (or (and (= sc 0) (< 3 sr 11))
                                                   (and (= sr 0) (< 3 sc 11)))
                                               (= c (1- cols))
                                               (= r (1- rows)))))
                                      (hash-table-alist heats))))))))))
