(ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201515
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201515)

(defparameter *example*
  (rutils:pairs->ht
   '((butterscotch
      (:capacity -1 :durability -2 :flavor 6 :texture 3 :calories 8))
     (cinnamon
      (:capacity 2 :durability 3 :flavor -2 :texture -1 :calories 3)))))

(defparameter *properties*
  '(:capacity :durability :flavor :texture))

(defun score (amounts ingredients)
  (loop
    :with totals := (list :capacity 0 :durability 0 :flavor 0 :texture 0)
    :for (ingredient . amount) :in amounts
    :do
       (loop :for property :in *properties*
             :do
                (incf (getf totals property)
                      (* amount (getf (gethash ingredient ingredients) property))))
    :finally
       (return
         (apply #'* (loop :for property :in *properties* :collect (max 0 (getf totals property)))))))

(defun possibilities (total parts)
  (cond
    ((zerop total)
     (list (loop :for i :below parts :collect 0)))
    ((zerop parts)
     (list (list)))
    ((= 1 parts)
     (list (list total)))
    (t
     (apply #'concatenate 'list
            (loop :for i :from 0 :to total
                  :collect (mapcar (curry #'cons i) (possibilities (- total i) (1- parts))))))))

(defun count-calories (amounts ingredients)
  (loop :for (name . amount) :in amounts
        :sum (* amount (getf (gethash name ingredients) :calories))))

(defun solve (ingredients)
  (let ((names (rutils:hash-table-keys ingredients)))
    (loop :for amounts :in (possibilities 100 (length names))
          :for ingredient-amounts := (mapcar #'cons names amounts)
          :when (= 500 (count-calories ingredient-amounts ingredients))
          :maximize (score ingredient-amounts ingredients))))

(defparameter *input*
  (rutils:pairs->ht
   '((sugar (:capacity 3 :durability 0 :flavor 0 :texture -3 :calories 2))
     (sprinkles (:capacity -3 :durability 3 :flavor 0 :texture 0 :calories 9))
     (candy (:capacity -1 :durability 0 :flavor 4 :texture 0 :calories 1))
     (chocolate (:capacity 0 :durability 0 :flavor -2 :texture 2 :calories 8)))))

(solve *input*) ;; 117936
