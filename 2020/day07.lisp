(defpackage #:day07
  (:use #:cl)
  (:import-from #:dop #:fn #:kw #:with-collecting #:counting #:collecting)
  (:import-from #:trivia #:match #:vector*)
  (:import-from #:rutils #:->> #:% #:xor #:it #:=> #:when-it)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day07)

;; (->> (aoc:puzzle 7)
;;      (str:lines)
;;      (subseq % 0 4)
;;      (mapcar #'parse-line))

(defun parse-words (words)
  (match words
    ((list a b "bags" "contain" "no" "other" "bags.")
     (cons (kw a b) nil))
    ((list* a b "bags" "contain" (read n) c d rest)
     (cons (kw a b) (cons (cons (kw c d) n)
                          (when (member (car rest) '("bag," "bags,") :test #'string=)
                            (parse-words rest)))))
    ((list* (or "bag," "bags,") (read n) a b rest)
     (cons (cons (kw a b) n)
           (when (member (car rest) '("bag," "bags,") :test #'string=)
             (parse-words rest))))))

(defun parse-rule (line)
  (destructuring-bind (key . vals) (parse-words (str:words line))
    (cons key (rtl:alist->ht vals))))

(defparameter +input+
  (->> (aoc:puzzle 7 2020)
       (str:lines)
       (mapcar #'parse-rule)
       rtl:alist->ht))

(let ((inverse #h()))
  (rtl:dotable (parent children +input+)
    (rtl:dotable (child count children)
      (pushnew parent (gethash child inverse))))

  (let ((parents (list :shiny-gold)))
    (with-collecting (uniq)
      (loop while (consp parents) do
        (dolist (parent (gethash (pop parents) inverse))
          (counting parent :as uniq)
          (push parent parents)))
      (rtl:ht-count uniq)))) ;; 265

(defun mega-count (children)
  `(+ ,@(mapcar #'cdr children)
      ,@(mapcar (fn ((cons name mul))
                    (let ((children))
                      (rtl:dotable (child count (gethash name +input+))
                        (push (cons child count) children))
                      (if children
                          (list '* mul (mega-count children))
                          0)))
                children)))

(->> (mega-count '((:shiny-gold . 1))) eval 1-) ;; 14177
