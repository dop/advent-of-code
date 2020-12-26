(defpackage #:day14
  (:use #:cl #:rutils))

(in-package #:day14)

(defun parse-mask (mask)
  (let ((and-mask (1- (expt 2 36)))
        (or-mask 0))
    (loop for i from (1- (length mask)) downto 0 do
      (case (elt mask i)
        (#\0 (decf and-mask (expt 2 (- 35 i ))))
        (#\1 (incf or-mask (expt 2 (- 35 i))))))
    (list and-mask or-mask)))

(defun parse-line (line)
  (if (string= (subseq line 0 3) "mem")
      (list :mem
            (parse-integer (subseq line 4 (position #\] line)))
            (parse-integer (subseq line (+ 2 (position #\= line)))))
      (cons :mask (parse-mask (subseq line (+ 2 (position #\= line)))))))

(defun parse-input (input)
  (->> input str:lines (mapcar #'parse-line)))

(defun solve (input)
  (let ((mem (make-hash-table))
        (and-mask (1- (expt 2 36)))
        (or-mask  0))
    (loop for (cmd . args) in input do
      (case cmd
        (:mask
         (setf and-mask (first args)
               or-mask (second args)))
        (:mem
         (setf (gethash (first args) mem)
               (logior (logand (second args) and-mask) or-mask)))))
    (reduce #'+ (ht-vals mem))))

(solve (parse-input (aoc:puzzle 14))) ;; 11501064782628

(defun parse-line-2 (line)
  (if (string= (subseq line 0 3) "mem")
      (list :mem
            (parse-integer (subseq line 4 (position #\] line)))
            (parse-integer (subseq line (+ 2 (position #\= line)))))
      (list :mask (subseq line (+ 2 (position #\= line))))))

(defun parse-input-2 (input)
  (->> input str:lines (mapcar #'parse-line-2)))

(defun targets (addr mask)
  (let ((addresses (list addr)))
    (loop for i from 0 to 35 do
      (case (elt mask (- 35 i))
        (#\1 (setf addresses
                   (loop for a in addresses
                         collect (logior a (expt 2 i)))))
        (#\X (setf addresses
                   (append (loop for a in addresses
                                 collect (logand a (- (1- (expt 2 36))
                                                      (expt 2 i))))
                           (loop for a in addresses
                                 collect (logior a (expt 2 i))))))))
    addresses))

(defun solve2 (input)
  (let ((mem (make-hash-table))
        (mask))
    (loop for (cmd . args) in input do
      (case cmd
        (:mask
         (setf mask (first args)))
        (:mem
         (loop for addr in (targets (first args) mask)
               do (setf (gethash addr mem) (second args))))))
    (reduce #'+ (ht-vals mem))))

(solve2 (parse-input-2 (aoc:puzzle 14))) ;; 5142195937660
