;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)

(defmacro with-output-buffer (name &rest body)
  (declare (indent 1))
  `(let ((standard-output (get-buffer-create ,name)))
     (with-current-buffer standard-output
       (erase-buffer))
     (print (progn ,@body))))

(defmacro with-puzzle (filepath &rest body)
  (declare (indent 1))
  `(with-output-buffer "*debug*"
     (with-current-buffer (find-file-noselect ,filepath)
       ,@body)))



;; day 11 part 1

(defun add! (set value)
  (setf (gethash value set) t))

(with-puzzle "~/tmp/day11.txt"
  (let* ((input (buffer-string))
         (univ (string-replace "\n" "" input))
         (len (length univ))
         (size (seq-position input ?\n))
         (rows (/ len size))
         (empty-rows (make-hash-table))
         (empty-cols (make-hash-table))
         (galaxies (make-hash-table :test 'equal)))

    ;; (let ((count (seq-count (lambda (c) (= c ?#)) univ)))
    ;;   (list len size rows (/ (* count (1- count)) 2) ))

    (loop for r from 0 below rows
          when (loop for c from 0 below size
                     never (= ?# (aref univ (+ c (* r size)))))
          do (add! empty-rows r))

    (loop for c from 0 below size
          when (loop for r from 0 below rows
                     never (= ?# (aref univ (+ c (* r size)))))
          do (add! empty-cols c))

    (loop for r from 0 below rows
          do (loop for c from 0 below size
                   when (= ?# (aref univ (+ c (* r size))))
                   do (add! galaxies (cons r c))))

    ;; (hash-table-alist galaxies)

    (loop for ((r . c) . rest) on (hash-table-keys galaxies)
          when rest
          sum (loop for (rr . cc) in rest
                    sum (+ (abs (- r rr))
                           (loop for er in (hash-table-keys empty-rows)
                                 count (< r er rr)
                                 count (> r er rr))
                           (abs (- c cc))
                           (loop for ec in (hash-table-keys empty-cols)
                                 count (< c ec cc)
                                 count (> c ec cc))))))) ;; 9684228



;; day 11 part 2

(with-puzzle "~/tmp/day11.txt"
  (let* ((input (buffer-string))
         (univ (string-replace "\n" "" input))
         (len (length univ))
         (size (seq-position input ?\n))
         (rows (/ len size))
         (empty-rows (make-hash-table))
         (empty-cols (make-hash-table))
         (galaxies (make-hash-table :test 'equal)))

    ;; (let ((count (seq-count (lambda (c) (= c ?#)) univ)))
    ;;   (list len size rows (/ (* count (1- count)) 2) ))

    (loop for r from 0 below rows
          when (loop for c from 0 below size
                     never (= ?# (aref univ (+ c (* r size)))))
          do (add! empty-rows r))

    (loop for c from 0 below size
          when (loop for r from 0 below rows
                     never (= ?# (aref univ (+ c (* r size)))))
          do (add! empty-cols c))

    (loop for r from 0 below rows
          do (loop for c from 0 below size
                   when (= ?# (aref univ (+ c (* r size))))
                   do (add! galaxies (cons r c))))

    ;; (hash-table-alist galaxies)

    (loop for ((r . c) . rest) on (hash-table-keys galaxies)
          when rest
          sum (loop for (rr . cc) in rest
                    sum (+ (abs (- r rr))
                           (* (1- 1000000)
                              (loop for er in (hash-table-keys empty-rows)
                                    count (< r er rr)
                                    count (> r er rr)))
                           (abs (- c cc))
                           (* (1- 1000000)
                              (loop for ec in (hash-table-keys empty-cols)
                                    count (< c ec cc)
                                    count (> c ec cc)))))))) ;; 483844716556
