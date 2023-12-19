;; -*- lexical-binding: t; -*-

(load (concat default-directory "aoc.el"))
(load (concat default-directory "heap.el"))



;; day 19 part 1

(with-puzzle "day19.txt"

  (let* ((parts (string-split (buffer-string) "\n\n" t))
         (code (make-hash-table :test 'equal)))
    ;; (pr "%s" parts)

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
                 (incf sum (apply #'+ (hash-table-values state)))))

          finally (return sum)))) ;; 397061

;; day 19 part 2

(with-puzzle "day19.txt"

  (let* ((part1 (car (string-split (buffer-string) "\n\n" t)))
         (code (make-hash-table :test 'equal)))

    ;; build code
    (loop for line in (string-lines part1)
          for (label . rules) = (string-split line "[{}:,]" t)
          do (setf (gethash label code)
                   (loop with rez
                         for (a b) on rules by #'cddr
                         when (and a b)
                         do (push (list (case (elt a 1) (?< '<) (?> '>))
                                        (case (elt a 0) (?x 'x) (?m 'm) (?a 'a) (?s 's))
                                        (string-to-number (subseq a 2))
                                        b)
                                  rez)
                         when (not b)
                         return (cons a (reverse rez)))))

    ;; run code
    (let ((Q '(("in" (x 1 4000) (m 1 4000) (a 1 4000) (s 1 4000))))
          (sum 0))
      (while Q
        (cl-destructuring-bind (id . state) (pop Q)
          ;; (pr "instruction %s, state %s" id state)
          (cond ((equal id "R"))
                ((equal id "A")
                 (incf sum (apply #'* (mapcar (lambda (var-range)
                                                (1+ (apply #'- (reverse (cdr var-range)))))
                                              state))))
                (t
                 (let* ((instructions (gethash id code))
                        (else (car instructions))
                        (rules (cdr instructions)))
                   ;; (pr "  rules: %s,\n  else: %s" rules else)
                   (loop for (op var num dest) in rules
                         for (_ l r) = (assoc var state)
                         ;; do (pr "    %s: %s %s %d %s=%d..%d" dest var op num var l r)
                         do (cond ((<= l num r)
                                   ;; split in two
                                   (cond
                                    ;; if <, add left to Q
                                    ((eq '< op)
                                     (push (cons dest (ressoc var (list l (1- num)) state)) Q)
                                     (setf state (ressoc var (list num r) state)))
                                    ;; if >, add right to Q
                                    ((eq '> op)
                                     (push (cons dest (ressoc var (list (1+ num) r) state)) Q)
                                     (setf state (ressoc var (list l num) state)))))
                                  ((or (and (< num l) (eq op '>))
                                       (and (< r num) (eq op '<)))
                                   ;; if <, skip
                                   ;; if >, add dest to Q
                                   (push (cons dest state) Q)
                                   (setf state nil)))
                         finally (when state
                                   (push (cons else state) Q))))))
          ;; (pr "Q: %s\n" (pp-to-string Q))
          ))

      sum))) ;; 125657431183201
