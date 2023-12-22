;; -*- lexical-binding: t; -*-

(load (expand-file-name "aoc.el" default-directory))
(load (car (file-expand-wildcards
            (expand-file-name "elpa/queue-*/queue.el" user-emacs-directory))))

;; day 21 part 1

(with-puzzle "day21.txt"
  (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
    (cl-macrolet ((ref (r c)
                    `(aref block (+ ,c (* ,r cols)))))
      (cl-labels ((valid-pos-p (r c)
                    (and (< -1 r rows) (< -1 c rows))))
        (let* ((start (seq-position block ?S))
               (pos (make-hash-table :test 'equal)))

          ;; (print-grid rows cols block)

          (set-add pos (list (/ start rows) (% start rows)))

          (loop repeat 65
                do (let ((next (make-hash-table :test 'equal)))
                     (loop for (r c) in (hash-table-keys pos)
                           do (loop for (dr dc) in '((-1 0) (1 0) (0 -1) (0 1))
                                    do (let ((nr (+ r dr))
                                             (nc (+ c dc)))
                                         (when (and (valid-pos-p nr nc)
                                                    (neq ?# (ref nr nc)))
                                           (set-add next (list nr nc)))))
                           do (setf pos next))))

          (hash-table-count pos)))))) ;; 3646

;; day 21 part 2

(comment
 (with-puzzle "day21.txt"
   (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
     (cl-macrolet ((ref (r c)
                     (let ((cc (gensym))
                           (rr (gensym)))
                       `(aref block (+ (let ((,cc (% ,c cols)))
                                         (if (< ,cc 0) (+ ,cc cols) ,cc))
                                       (* (let ((,rr (% ,r rows)))
                                            (if (< ,rr 0) (+ ,rr rows) ,rr))
                                          cols))))))
       (cl-labels ((valid-pos-p (r c)
                     (and (< -1 r rows) (< -1 c rows))))
         ;; (print-grid rows cols block)

         (let* ((start (seq-position block ?S))
                (seen (make-hash-table :test 'equal))
                (answer 0)
                (Q (make-queue))
                (even 0)
                (odd 0))

           (queue-enqueue Q (list 0 (/ start rows) (% start rows)))

           (loop with limit = (+ 65 (* 131 2))
                 until (queue-empty Q)
                 for (i r c) = (queue-dequeue Q)
                 do (when (not (gethash (cons r c) seen))
                      ;; (pr "%d. %d %d" i r c)
                      (setf (gethash (cons r c) seen) i)
                      (if (evenp i)
                          (incf even)
                        (incf odd))
                      (when (< i limit)
                        (loop for (dr dc) in '((-1 0) (1 0) (0 -1) (0 1))
                              do (let ((nr (+ r dr))
                                       (nc (+ c dc)))
                                   (when (neq ?# (ref nr nc))
                                     ;; (pr "  enqueue %d %d" nr nc)
                                     (queue-enqueue Q (list (1+ i) nr nc))))))))

           (list even odd))))))

 ;; 1 (33496 33157)
 ;; 2 (92292 92857)

 ;; middle odd  3759
 ;; middle even 3646

 ;; block odd  7388
 ;; block even 7424

 )

 (with-puzzle "day21.txt"
   (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
     (cl-macrolet ((ref (r c)
                     `(aref block (+ ,c (* ,r cols)))))
       ;; (print-grid rows cols block)

       (cl-labels ((valid-pos-p (r c)
                     (and (< -1 r rows) (< -1 c rows))))
         (let* ((start (seq-position block ?S))
                (seen (make-hash-table :test 'equal))
                (answer 0)
                (Q (make-queue)))

           (queue-enqueue Q (list 0 (/ start rows) (% start rows)))

           (loop until (queue-empty Q)
                 for (i r c) = (queue-dequeue Q)
                 do (when (not (gethash (cons r c) seen))
                      (setf (gethash (cons r c) seen) i)
                      (loop for (dr dc) in '((-1 0) (1 0) (0 -1) (0 1))
                            do (let ((nr (+ r dr))
                                     (nc (+ c dc)))
                                 (when (and (valid-pos-p nr nc)
                                            (neq ?# (ref nr nc)))
                                   (queue-enqueue Q (list (1+ i) nr nc)))))))

           (let* ((O 0)  ;; Odd block
                  (o 0)  ;; Odd center
                  (E 0)  ;; Even block
                  (e 0)) ;; Even center
             (loop for steps in (hash-table-values seen) do
                   (cond ((oddp steps)
                          (incf O)
                          (when (<= steps 65)
                            (incf o)))
                         (t
                          (incf E)
                          (when (<= steps 65)
                            (incf e)))))

             (pr "odd %d" O)
             (pr "even %d" E)
             (pr "odd center %d" o)
             (pr "even center %d" e)

             (let ((E 7424)
                   (e 3646)
                   (O 7388)
                   (o 3759)
                   (n (1+ (/ 26501365 rows))))

               (+ (* E (^ (1- n) 2))
                  (* O (^ n 2))
                  (* (1- n) (- E e))
                  (- (* n (- O o))))))))))) ;; 606188414811259
