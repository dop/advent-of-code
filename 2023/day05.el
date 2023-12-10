(require 'cl)

(add-to-list 'load-path "/Users/dop/.emacs.d/elpa/dash-20221013.836")
(require 'dash)

;; day 5 part 1
(with-current-buffer (find-file-noselect "~/tmp/day5.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " "))))
    (destructuring-bind (seeds . maps) (string-split (buffer-string) "\n\n+")
      (let ((seeds
             (apply #'vector
                    (mapcar #'number (cdr (string-split seeds " ")))))
            (mappings
             (mapcar (lambda (map) (mapcar #'numbers (cdr (string-lines map)))) maps)))

        (loop for mapping in mappings
              do (loop for i from 0 below (length seeds)
                       do (loop for (dst src range) in mapping
                                for seed = (elt seeds i)
                                when (<= src seed (1- (+ src range)))
                                do (progn
                                     (setf (elt seeds i)
                                           (+ seed (- dst src)))
                                     (return)))))
        (apply #'min (append seeds nil)))))) ;; 484023871



(defun shift-range (range mapping)
  (let* ((a (car range))
         (b (+ a (cdr range) -1))
         (x (cadr mapping))
         (y (+ x (caddr mapping) -1))
         (delta (- (car mapping) x)))
    (->> (cond
          ((<= x a y b)
           (list (cons (+ a delta) (+ y delta))
                 (cons (1+ y) b)))
          ((<= a x b y)
           (list (cons (+ x delta) (+ b delta))
                 (cons a (1- x))))
          ((<= x a b y)
           (list (cons (+ a delta) (+ b delta))))
          ((<= a x y b)
           (list (cons (+ x delta) (+ y delta))
                 (cons a (1- x))
                 (cons (1+ y) b)))
          (t
           (list nil (cons a b))))
         (seq-map (lambda (pair)
                    (when pair
                      (cons (car pair) (1+ (- (cdr pair) (car pair)))))))
         (seq-filter (lambda (pair)
                       (if pair (plusp (cdr pair)) t))))))

;; outside
(shift-range '(10 . 3) '(0 8 2)) ;; (nil (10 . 3))
(shift-range '(10 . 3) '(0 13 2)) ;; (nil (10 . 3))

;; middle
(shift-range '(10 . 6) '(0 12 2)) ;; ((0 . 2) (10 . 2) (14 . 2))

;; right exact
(shift-range '(10 . 6) '(0 14 2)) ;; ((0 . 2) (10 . 4))

;; left exact
(shift-range '(10 . 6) '(0 10 2)) ;; ((0 . 2) (12 . 4))

;; equal
(shift-range '(10 . 6) '(0 10 6)) ;; ((0 . 6))

;; right overlap
(shift-range '(10 . 6) '(0 12 6)) ;; ((0 . 4) (10 . 2))

;; left overlap
(shift-range '(10 . 6) '(0 8 6)) ;; ((2 . 4) (14 . 2))

;; overlap
(shift-range '(10 . 6) '(0 8 10)) ;; ((2 . 6))

;; day 5 part 2
(with-current-buffer (find-file-noselect "~/tmp/day5.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " "))))
    (destructuring-bind (seeds . maps) (string-split (buffer-string) "\n\n+")
      (let ((seeds
             (mapcar (lambda (list) (cons (car list) (cadr list)))
                     (-partition 2 (mapcar #'number (cdr (string-split seeds " "))))))
            (stages
             (mapcar (lambda (map) (mapcar #'numbers (cdr (string-lines map)))) maps)))
        (->>
         (seq-reduce (lambda (seeds stage)
                       ;; (message "--- STAGE")
                       (let ((result
                              (seq-reduce (lambda (state mapping)
                                            (let ((mapped nil)
                                                  (unmapped nil))
                                              ;; (message "mapping: %s" mapping)
                                              ;; (message "processed: %s" (car state))
                                              ;; (message "unprocessed: %s" (cdr state))
                                              (loop for range in (cdr state)
                                                    do (let ((result (shift-range range mapping)))
                                                         (when (car result)
                                                           (push (car result) mapped))
                                                         (setf unmapped (append unmapped (cdr result)))))
                                              ;; (message " -> %s" mapped)
                                              ;; (message " -> %s" unmapped)
                                              (cons (append (car state) mapped) unmapped)))
                                          stage ;; mappings
                                          (cons nil seeds))))
                         (append (car result) (cdr result))))
                     stages
                     seeds)
         (mapcar #'car)
         (apply #'min)))))) ;; 46294175
