;; -*- lexical-binding: t; -*-

(load "~/Projects/advent-of-code/2023/aoc.el")
(load "~/Projects/advent-of-code/2023/heap.el")



;; day 19 part 1

(with-puzzle "day19.txt"

  (let* ((parts (string-split (buffer-string) "\n\n" t))
         (code (make-hash-table :test 'equal)))
    (pr "%s" parts)

    ;; build code
    (loop for line in (string-lines (elt parts 0))
          for (label . rules) = (string-split line "[{}:,]" t)
          do (setf (gethash label code)
                   (loop for (a b) on rules by #'cddr
                         when (and a b)
                         collect (list (elt a 1)
                                       (elt a 0)
                                       (string-to-number (subseq a 2))
                                       b)
                         when (not b)
                         collect a)))

    ;; run code
    (loop with sum = 0
          for line in (string-lines (elt parts 1))
          for state = (loop with st = (make-hash-table)
                            for (var val) on (string-split line "[{},=]" t) by #'cddr
                            do (setf (gethash (elt var 0) st) (string-to-number val))
                            finally (return st))
          do (let ((i "in"))
               (while (not (or (equal i "A") (equal i "R")))
                 (loop for rule in (gethash i code)
                       when (atom rule)
                       do (return (setf i rule))
                       for (op var val ni) = rule
                       when (funcall (case op (?< #'<) (?> #'>))
                                     (gethash var state)
                                     val)
                       do (return (setf i ni))))
               (when (equal i "A")
                 (pr "%s" (hash-table-values state))
                 (incf sum (apply #'+ (hash-table-values state)))))

          finally (return sum))))


;; day 19 part 2

(with-puzzle "day19.txt"
  )
