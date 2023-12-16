;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)

(defmacro with-output-buffer (name &rest body)
  (declare (indent 1))
  `(let ((standard-output (if (stringp ,name)
                              (get-buffer-create ,name)
                            ,name)))
     (when (bufferp standard-output)
       (with-current-buffer standard-output
         (erase-buffer)
         (redisplay)))
     (print (progn ,@body))))

(defmacro with-puzzle (options &rest body)
  (declare (indent 1))
  `(with-output-buffer
       ,(cl-typecase options
          (string "*debug*")
          (t (getf options :out "*debug*")))
     (with-current-buffer
         (find-file-noselect ,(cl-typecase options
                                (string options)
                                (t (getf options :in))))
       ,@body)))



;; day 14 part 1

(defalias 'partial #'apply-partially)

(defun read-grid (raw)
  (let* ((grid (string-trim raw))
         (cols (seq-position grid ?\n))
         (rows (1+ (seq-count (partial #'eq ?\n) grid)))
         (block (string-replace "\n" "" grid)))
    (list rows cols block)))

(defun north-load (rows cols block)
  (loop for c from 0 below cols
        sum (loop with w = rows
                  for r from 0 below rows
                  for x = (aref block (+ c (* r cols)))
                  when (eq ?O x) sum (prog1 w (decf w))
                  when (eq ?# x) do (setf w (- rows r 1)))))

(with-puzzle (:in "~/tmp/day14.txt")
  (apply #'north-load (read-grid (buffer-string)))) ;; 108759



;; day 14 part 2

(defun print-grid (rows cols block)
  (loop for r from 0 below rows
        do (loop for c from 0 below cols
                 do (princ (format "%c" (aref block (+ c (* r cols))))))
        do (princ "\n")))

(defun pr (template &rest args)
  (princ (apply #'format template args))
  (terpri))

(defun neq (a b) (not (eq a b)))

(defun calc-load (rows cols block)
  (loop for c from 0 below cols
        sum (loop for r from 0 below rows
                  for x = (aref block (+ c (* r cols)))
                  when (eq ?O x) sum (- rows r))))

(defun counthash (key table)
  (setf (gethash key table) (1+ (gethash key table 0))))

(with-puzzle (:in "~/tmp/day14.example.txt")
  (pcase (read-grid (buffer-string))
    (`(,rows ,cols ,block)
     (cl-macrolet ((ref (r c)
                     `(aref block (+ ,c (* ,r cols)))))

       ;; (pr "\n-- initial --\n")
       ;; (print-grid rows cols block)
       ;; (pr "\nload: %d" (calc-load rows cols block))

       (let ((counts (make-hash-table)))
         (dotimes (i 100)
           ;; (pr "\n-- cycle %d --\n" i)
           ;; (print-grid rows cols block)

           (let ((load (calc-load rows cols block)))
             (pr "%d. %d %d" i load (counthash load counts)))

           (progn
             ;; (pr "\n-- north --\n")
             (loop for c from 0 below cols
                   do (loop with e = 0
                            for r from 0 below rows
                            for x = (ref r c)
                            ;; see ball, saw empty place before
                            ;; do (pr "%d. %c %d" r x e)
                            when (eq ?O x)
                            do (progn
                                 (when (< e r)
                                   (setf (ref e c) ?O
                                         (ref r c) ?.))
                                 (incf e))

                            ;; see block, need to find next empty space
                            when (eq ?# x)
                            do (loop for i from r below rows
                                     when (neq ?# (ref i c))
                                     do (return (setf e i))
                                     finally (setf e i))
                            ;; see empty space
                            ;; when (and (eq ?. x) (< e r))
                            ;; do
                            ))
             ;; (print-grid rows cols block)
             ;; (push (calc-load rows cols block) rez)
             ;; (pr "\nload: %d" (calc-load rows cols block))
             )

           (progn
             ;; (pr "\n-- west --\n")
             (loop for r from 0 below rows
                   do (loop with e = 0
                            for c from 0 below rows
                            for x = (ref r c)
                            ;; see ball, saw empty place before
                            ;; do (pr "%d. %c %d" c x e)
                            when (eq ?O x)
                            do (progn
                                 (when (< e c)
                                   (setf (ref r e) ?O
                                         (ref r c) ?.))
                                 (incf e))

                            ;; see block, need to find next empty space
                            when (eq ?# x)
                            do (loop for i from c below cols ;;  c downto 0
                                     when (neq ?# (ref r i))
                                     do (return (setf e i))
                                     finally (setf e i))
                            ;; see empty space
                            ;; when (and (eq ?. x) (< e r))
                            ;; do
                            ))
             ;; (print-grid rows cols block)
             ;; (pr "\nload: %d" (calc-load rows cols block))
             ;; (push (calc-load rows cols block) rez)
             )

           (progn
             ;; (princ "\n-- south --\n\n")
             (loop for c from 0 below cols
                   do (loop with e = (1- rows)
                            for r from (1- rows) downto 0
                            for x = (ref r c)
                            ;; see ball, saw empty place before
                            ;; do (pr "%d. %c %d" r x e)
                            when (eq ?O x)
                            do (progn
                                 (when (> e r)
                                   (setf (ref e c) ?O
                                         (ref r c) ?.))
                                 (decf e))

                            ;; see block, need to find next empty space
                            when (eq ?# x)
                            do (loop for i from r downto 0
                                     when (neq ?# (ref i c))
                                     do (return (setf e i))
                                     finally (setf e i))
                            ;; see empty space
                            ;; when (and (eq ?. x) (< e r))
                            ;; do
                            ))
             ;; (print-grid rows cols block)
             ;; (pr "\nload: %d" (calc-load rows cols block))
             ;; (push (calc-load rows cols block) rez)
             )

           (progn
             ;; (pr "\n-- east --\n")
             (loop for r from 0 below rows
                   do (loop with e = (1- cols)
                            for c from (1- cols) downto 0
                            for x = (ref r c)
                            ;; see ball, saw empty place before
                            ;; do (pr "%d. %c %d" r x e)
                            when (eq ?O x)
                            do (progn
                                 (when (> e c)
                                   (setf (ref r e) ?O
                                         (ref r c) ?.))
                                 (decf e))

                            ;; see block, need to find next empty space
                            when (eq ?# x)
                            do (loop for i from c downto 0
                                     when (neq ?# (ref r i))
                                     do (return (setf e i))
                                     finally (setf e i))
                            ;; see empty space
                            ;; when (and (eq ?. x) (< e r))
                            ;; do
                            ))
             ;; (print-grid rows cols block)
             ;; (push (calc-load rows cols block) rez)
             ;; (pr "\nload: %d" (calc-load rows cols block))
             )))))))

(% (- 1000000000 150) 17) ;; 10

(% (- 1000000000 2) 7) ;; 4

;; 89089
