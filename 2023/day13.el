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



;; day 13 part 1

(defun seq-count-eq (el sequence)
  (seq-count (lambda (e) (eq el e)) sequence))

(defun palindrome-p (seq)
  (let ((len (length seq)))
    (loop for i from 0 below (/ len 2)
          always (eq (elt seq i) (elt seq (- len i 1))))))

(with-puzzle (:in "~/tmp/day13.txt")
  (loop
   for grid in (string-split (buffer-string) "\n\n" t "\n")
   sum
   (let ((sum 0)
         (cols (seq-position grid ?\n))
         (rows (1+ (seq-count-eq ?\n grid)))
         (block (string-replace "\n" "" grid)))
     ;; (princ grid) (princ "\n")
     ;; (print (list :cols cols :rows rows))

     (loop for c from 1 below cols
           do (let ((span (min c (- cols c))))
                (when (loop for r from 0 below rows
                            always (let* ((from (+ (- c span) (* r cols)))
                                          (to   (+ (+ c span) (* r cols)))
                                          (seq (seq-subseq block from to)))
                                     (palindrome-p seq)))
                  (incf sum c)
                  (return))))

     (loop for r from 1 below rows
           do (let ((span (min r (- rows r))))
                (when (loop for c from 0 below cols
                            always (let ((seq (loop for i from (- r span) below (+ r span)
                                                    collect (aref block (+ c (* i cols))))))
                                     (palindrome-p seq)))
                  (incf sum (* 100 r))
                  (return))))

     sum))) ;; 29846



;; day 13 part 2

(defun neq (a b) (not (eq a b)))

(defun palindrome-miss-count (seq)
  (let ((len (length seq)))
    (loop for i from 0 below (/ len 2)
          count (neq (elt seq i) (elt seq (- len i 1))))))

(with-puzzle (:in "~/tmp/day13.txt")
  (loop
   for grid in (string-split (buffer-string) "\n\n" t "\n")
   sum
   (let ((sum 0)
         (cols (seq-position grid ?\n))
         (rows (1+ (seq-count-eq ?\n grid)))
         (block (string-replace "\n" "" grid)))
     ;; (princ grid) (princ "\n")
     ;; (print (list :cols cols :rows rows))

     (loop for c from 1 below cols
           do (let ((span (min c (- cols c))))
                (when (= 1 (loop for r from 0 below rows
                                 sum (let* ((from (+ (- c span) (* r cols)))
                                            (to   (+ (+ c span) (* r cols)))
                                            (seq (seq-subseq block from to)))
                                       (palindrome-miss-count seq))))
                  (incf sum c)
                  (return))))

     (loop for r from 1 below rows
           do (let ((span (min r (- rows r))))
                (when (= 1 (loop for c from 0 below cols
                                 sum (let ((seq (loop for i from (- r span) below (+ r span)
                                                      collect (aref block (+ c (* i cols))))))
                                       (palindrome-miss-count seq))))
                  (incf sum (* 100 r))
                  (return))))

     sum))) ;; 25401
