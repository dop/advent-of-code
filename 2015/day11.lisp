(ql:quickload '(:alexandria :rutils :str))

(defpackage #:aoc201511
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:if-it #:getsethash))

(in-package #:aoc201511)

(defun group-by (fn list)
  (when list
    (loop
      :with groups := (list '())
      :for (a b) :on list :do
         (push a (car groups))
         (unless (or (not b) (funcall fn a b))
           (push '() groups))
      :finally
         (return (nreverse groups)))))

(defun rle (list)
  (mapcar (lambda (group) (list (car group) (length group)))
          (group-by #'eq list)))

(defun next (number)
  (write-to-string (1+ (parse-integer number :radix 36)) :base 36))

(defun has-straight-p (number)
  (loop :for (a b c) :on number :by #'cdr
        :when (and a b c (= (+ 2 (char-code a)) (+ 1 (char-code b)) (char-code c)))
        :collect (list a b c)))

(defun has-iol-p (number)
  (loop :for digit :in number :thereis (member digit '(#\i #\o #\l #\I #\O #\L))))

(defun has-two-pairs-p (number)
  (<= 2 (loop :for (digit count) :in (rle number)
              :count (<= 2 count))))

(defun elf-secure-password-p (password)
  (let ((digits (coerce password 'list)))
    (and (every #'alpha-char-p digits)
         (has-straight-p digits)
         (not (has-iol-p digits))
         (has-two-pairs-p digits))))

(loop
  :for password := (next "CQJXXYZZ") :then (next password)
  :until (elf-secure-password-p password)
  :finally (return password))

;; "cqjxxyzz"

;; "cqkaabcc"
