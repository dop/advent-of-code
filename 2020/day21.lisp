(defpackage #:day21
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry))

(in-package #:day21)

(defmacro while-it (expr &body body)
  `(loop for it = ,expr while it do ,@body))

(defparameter *example*
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(defun parse-line (line)
  (let* ((brace-pos (position #\( line))
         (ingredients (str:words (subseq line 0 (1- brace-pos))))
         (allergens-string (subseq line brace-pos))
         (space-pos (position #\space allergens-string))
         (allergens (str:split ", "
                               (subseq allergens-string (1+ space-pos)
                                       (1- (length allergens-string))))))
    (list ingredients allergens)))

(defun add-ingredients (map allergen ingredients)
  (unless (nth-value 1 (gethash allergen map))
    (setf (gethash allergen map) (make-hash-table :test 'equal)))
  (loop for ing in ingredients do
    (if (nth-value 1 (gethash ing (gethash allergen map)))
        (incf (gethash ing (gethash allergen map)))
        (setf (gethash ing (gethash allergen map)) 1))))

(defun solve1 (input)
  (let ((result (make-hash-table :test 'equal))
        (answer))
    (with-input-from-string (in input)
      (while-it (read-line in nil nil)
        (destructuring-bind (ingredients allergens) (parse-line it)
          (setf answer (append answer ingredients))
          (loop for allergen in allergens do
            (add-ingredients result allergen ingredients)))))

    (dotable (al ings result)
      (destructuring-bind ((bad-ing . _) . rest-ings) (sort (ht->alist ings) #'> :key #'cdr)
        (setf answer (remove bad-ing answer :test #'equal))
        (dotable (al ings2 result)
          (remhash bad-ing ings2))))

    (length answer)))

(solve1 (dop:puzzle)) ;; 2098

(defun allergens-ingredients-map (pairs)
  (let ((result (make-hash-table :test 'equal)))
    (loop for (ingredients allergens) in pairs do
      (loop for allergen in allergens do
        (add-ingredients result allergen ingredients)))
    result))

(defun sort-options (options)
  (sort options
        (lambda (a b)
          (destructuring-bind ((a1 (ing1 count1) . r1)
                               (a2 (ing2 count2) . r2))
              (list a b)
            (declare (ignore a1 a2 ing1 ing2))
            (or (< (length r1) (length r2))
                (and (= (length r1) (length r2))
                     (> count1 count2)))))))

(defun remove-ingredient (ing options)
  (loop for (a . ingredients) in options
        collect (cons a (remove-if #`((equal ing (car %))) ingredients))))

(defun resolve (options)
  (when options
    (destructuring-bind (option . rest-of-options) (sort-options options)
      (destructuring-bind (a (ing . count) . _) option
        (declare (ignore _ count))
        (cons (cons a ing)
              (resolve (remove-ingredient ing rest-of-options)))))))

(defun solve2 (input)
  (->> (allergens-ingredients-map (mapcar #'parse-line (str:lines input)))
       (mapkv #`(first (dop:group-by (sort (ht->pairs %%) #'> :key #'second) :key #'second)))
       (ht->alist)
       (resolve)
       (sort % #'string< :key #'car)
       (mapcar #'cdr)
       (str:join ",")))

(solve2 (dop:puzzle)) ;; "ppdplc,gkcplx,ktlh,msfmt,dqsbql,mvqkdj,ggsz,hbhsx"
