(defpackage #:day10
  (:use #:cl)
  (:import-from #:dop #:fn #:kw #:>>)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:-> #:->> #:% #:%%  #:2nd #:xor #:it #:=> #:when-it #:if-it
                #:ht->alist #:ht->plist #:ht-vals #:ht-keys #:dotable
                #:fmt #:length=)
  (:import-from #:alexandria #:when-let #:when-let* #:symbolicate)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day10)

(named-readtables:in-readtable :aoc)

(defun to-diffs (adapters)
  (loop for (i j) on (sort (copy-seq adapters) #'<)
        when j collect (- j i)))

(defun group (sequence &key (test #'=))
  (->> (reduce (lambda (acc x)
                 (if (or (null (car acc))
                         (funcall test (caar acc) x))
                     (cons (cons x (car acc)) (cdr acc))
                     (cons (list x) acc)))
               sequence
               :initial-value (list nil))
       nreverse))

(defparameter +input+
  (->> (aoc:puzzle 10) str:lines (mapcar #'parse-integer) (sort % #'<)))

(defun 1= (x &rest xs)
  (and (= 1 x)
       (if xs (apply #'1= (car xs) (cdr xs)) t)))

(defun solve (list)
  (match list
    ((list* a b rest)
     (cond ((< (- b a) 4)
            (if rest
                (append (solve (cons a rest))
                        (mapcar #`(cons a %)
                                (solve (cons b rest))))
                (mapcar #`(cons a %)
                        (solve (cons b rest)))))
           (t
            nil)))
    ((list* x)
     (list x))))

(defparameter +example-1+
  '(16 10 15 5 1 11 7 19 6 12 4))

(defparameter +example-2+
  '(28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))

(defun prep-input (input)
  (sort (cons 0 (cons (+ 3 (reduce #'max input)) (copy-seq input))) #'<))

(length (solve (prep-input +example-1+))) ;; 8

(length (solve (prep-input +example-2+))) ;; 19208

(defun solve-list (list)
  (match list
    ((list* a b rest)
     (if (> (- b a) 3)
         0
         (if rest
             (+ (solve-list (cons a rest))
                (solve-list (cons b rest)))
             1)))
    ((list* _)
     1)))

(defun solve-vec (i j vec)
  (if (< i j (length vec))
      (let ((a (elt vec i))
            (b (elt vec j)))
        (if (< (- b a) 4)
            (if (< j (1- (length vec)))
                (+ (solve-vec i (1+ j) vec)
                   (solve-vec j (1+ j) vec))
                1)
              0))
      1))

(defun cached-solve-vec (i j vec)
  (if (> (aref *cache* i j) -1)
      (aref *cache* i j)
      (setf (aref *cache* i j)
            (if (< i j (length vec))
                (let ((a (elt vec i))
                      (b (elt vec j)))
                  (if (< (- b a) 4)
                      (if (< j (1- (length vec)))
                          (+ (cached-solve-vec i (1+ j) vec)
                             (cached-solve-vec j (1+ j) vec))
                          1)
                      0))
                1))))

(let* ((input (coerce (prep-input +input+) 'vector))
       (*cache* (make-array (list (length input) (length input)) :initial-element -1)))
  (print (cached-solve-vec 0 1 input)))
