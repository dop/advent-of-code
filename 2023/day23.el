;; -*- lexical-binding: t; -*-

(load (expand-file-name "lisp/my-library.el" user-emacs-directory))
(load (expand-file-name "aoc.el" default-directory))
(load (expand-file-name "heap.el" default-directory))
(load (car (file-expand-wildcards
            (expand-file-name "elpa/queue-*/queue.el" user-emacs-directory))))

;; day 23 part 1

(with-puzzle "day23.txt"
  (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
    ;; (print-grid rows cols block)
    (cl-macrolet ((ref (r c)
                    `(aref block (+ ,c (* ,r cols)))))
      (let ((target (list (1- rows) (- cols 2)))
            (start  (list 0 1))
            (V (make-hash-table :test 'equal))
            (E (make-hash-table :test 'equal))
            (W (make-hash-table :test 'equal)))

        (cl-labels ((valid-pos-p (r c)
                      (and (< -1 r rows) (< -1 c cols)))
                    (seen-p (id r c)
                      (gethash (list id r c) seen)))

          (assert (eq ?. (ref 0 1)))
          (assert (eq ?. (ref (elt target 0) (elt target 1))))

          (set-add V target)
          (set-add V start)

          (let ((Q (make-queue)))
            (queue-enqueue Q (list 1   ;; distance
                                   0 1 ;; start
                                   0 1 ;; previous
                                   1 1 ;; current
                                   ))

            (loop until (queue-empty Q)
                  for (len r0 c0 pr pc r c) = (queue-dequeue Q)
                  ;; unless (gethash (list r c) seen)
                  do
                  ;; (pri len r0 c0 pr pc r c)
                  ;; (setf (gethash (list r c) seen) t)
                  (if (equal target (list r c))
                      (progn
                        (push target (gethash (list r0 c0) E))
                        (setf (gethash (list* r0 c0 target) W) len))
                    (let (next)
                      (loop for (dr dc) in '((0 1) (1 0) (0 -1) (-1 0))
                            do (let ((nr (+ r dr))
                                     (nc (+ c dc)))
                                 ;; (pri nr nc)
                                 (when (and (valid-pos-p nr nc)
                                            (if (< dr 0) (neq (ref r c) ?v) t) ;; part
                                            (if (< dc 0) (neq (ref r c) ?>) t) ;; 1
                                            (neq ?# (ref nr nc))
                                            (or (neq nr pr) (neq nc pc)))
                                   (push (list nr nc) next))))
                      ;; (pri next)
                      (cond ((< 1 (length next))
                             (let ((n (list r c)))
                               (pushnew n (gethash (list r0 c0) E) :test #'equal)
                               (setf (gethash (list r0 c0 r c) W) len)
                               (unless (gethash n V)
                                 (set-add V n)
                                 (loop for (nr nc) in next
                                       do (queue-enqueue Q (list 1 r c r c nr nc))))))
                            ((= 1 (length next))
                             (let ((nr (caar next))
                                   (nc (cadar next)))
                               (queue-enqueue Q (list (1+ len) r0 c0 r c nr nc)))))))))

          ;; (pri V)
          ;; (pri E)
          ;; (pri W)

          (cl-labels ((dfs (v)
                        (if (equal v target)
                            0
                            (loop with best = 0
                                  for n in (gethash v E)
                                  do (setf best (max best (+ (gethash (append v n) W)
                                                             (dfs n))))
                                  finally (return best)))))
            (dfs start))))))) ;; => 2278 in 0.091189 sec.

;; part 2

(with-puzzle "day23.txt"
  (cl-destructuring-bind (rows cols block) (read-grid (buffer-string))
    ;; (print-grid rows cols block)
    (cl-macrolet ((ref (r c)
                    `(aref block (+ ,c (* ,r cols)))))
      (let ((target (list (1- rows) (- cols 2)))
            (start  (list 0 1))
            (V (make-hash-table :test 'equal))
            (E (make-hash-table :test 'equal))
            (W (make-hash-table :test 'equal)))

        (cl-labels ((valid-pos-p (r c)
                      (and (< -1 r rows) (< -1 c cols)))
                    (seen-p (id r c)
                      (gethash (list id r c) seen)))

          (assert (eq ?. (ref 0 1)))
          (assert (eq ?. (ref (elt target 0) (elt target 1))))

          (set-add V target)
          (set-add V start)

          (let ((Q (make-queue)))
            (queue-enqueue Q (list 1   ;; distance
                                   0 1 ;; start
                                   0 1 ;; previous
                                   1 1 ;; current
                                   ))

            (loop until (queue-empty Q)
                  for (len r0 c0 pr pc r c) = (queue-dequeue Q)
                  ;; unless (gethash (list r c) seen)
                  do
                  ;; (pri len r0 c0 pr pc r c)
                  ;; (setf (gethash (list r c) seen) t)
                  (if (equal target (list r c))
                      (progn
                        (push target (gethash (list r0 c0) E))
                        (setf (gethash (list* r0 c0 target) W) len))
                    (let (next)
                      (loop for (dr dc) in '((0 1) (1 0) (0 -1) (-1 0))
                            do (let ((nr (+ r dr))
                                     (nc (+ c dc)))
                                 ;; (pri nr nc)
                                 (when (and (valid-pos-p nr nc)
                                            ;; (if (< dr 0) (neq (ref r c) ?v) t) ;; part
                                            ;; (if (< dc 0) (neq (ref r c) ?>) t) ;; 1
                                            (neq ?# (ref nr nc))
                                            (or (neq nr pr) (neq nc pc)))
                                   (push (list nr nc) next))))
                      ;; (pri next)
                      (cond ((< 1 (length next))
                             (let ((n (list r c)))
                               (pushnew n (gethash (list r0 c0) E) :test #'equal)
                               (pushnew (list r0 c0) (gethash n E) :test #'equal)
                               (setf (gethash (list r0 c0 r c) W) len)
                               (setf (gethash (list r c r0 c0) W) len)
                               (unless (gethash n V)
                                 (set-add V n)
                                 (loop for (nr nc) in next
                                       do (queue-enqueue Q (list 1 r c r c nr nc))))))
                            ((= 1 (length next))
                             (let ((nr (caar next))
                                   (nc (cadar next)))
                               (queue-enqueue Q (list (1+ len) r0 c0 r c nr nc)))))))))

          ;; (pri V)
          ;; (pri E)
          ;; (pri W)

          (let ((best 0))
            (cl-labels ((distance (path)
                          (loop for (a b) on path
                                when b sum (gethash (append a b) W)))
                        (dfs (v seen)
                          ;; (pri v seen)
                          (cond ((equal v target)
                                 (let ((dist (distance (reverse (cons v seen)))))
                                   (when (< best dist)
                                     (setf best dist)
                                     (pri best))
                                   dist))
                                ((member* v seen :test #'equal) 0)
                                (t
                                 (loop for n in (gethash v E)
                                       maximize (dfs n (cons v seen)))))))
              (dfs start nil)))))))) ;; => 6734 in 102.997271 sec.
