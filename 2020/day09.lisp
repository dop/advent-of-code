(defpackage #:day09
  (:use #:cl)
  (:import-from #:dop #:fn #:kw #:>>)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:-> #:->> #:% #:%%  #:2nd #:xor #:it #:=> #:when-it #:if-it
                #:ht->alist #:ht->plist #:ht-vals #:ht-keys #:dotable
                #:fmt #:length=)
  (:import-from #:alexandria #:when-let #:when-let* #:symbolicate)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day09)

(defparameter +input+
  (->> (aoc:puzzle 9 2020)
       str:lines
       (mapcar #'parse-integer)
       (make-array (length %) :initial-contents % :adjustable t)))

(defun dumb-search (target arr)
  (loop for i from 0 below (1- (length arr)) do
    (loop for j from (1+ i) below (length arr) do
      (when (= target (+ (elt arr i) (elt arr j)))
        (return-from dumb-search t)))))

(defun double-pointer-search (target arr)
  (loop
    with i = 0 and j = (1- (length arr))
    while (< i j) do
      (let* ((a (elt arr i))
             (b (elt arr j))
             (sum (+ a b)))
        (cond ((= sum target)
               (return t))
              ((< sum target)
               (incf i))
              (t
               (decf j))))))

(defun solve (input)
  (let ((preamble (make-array 25 :initial-contents (sort (copy-seq (subseq input 0 25)) #'<)
                                 :adjustable t))
        (rest (subseq input 25)))
    (loop for n across rest do
      (if (dumb-search n preamble)
          (progn
            (vector-push-extend n preamble)
            (setf preamble (sort preamble #'<)))
          (return-from solve n)))))

(solve +input+) ;; 15353384

(defparameter +cache+
  (make-array (* 1000 1000) :initial-element 0))

(defun range-sum (i j)
  (if (= i j)
      (elt +input+ j)
      (+ (elt +input+ j)
         (range-sum i (1- j)))))

(loop named main for i from 0 below 999 do
  (loop for j from (1+ i) below 1000 do
    (when (= 15353384 (range-sum i j))
      (return-from main (cons i j)))))

(let ((range (sort (copy-seq (subseq +input+ 384 401)) #'<)))
  (+ (elt range 0)
     (elt range (1- (length range))))) ;; 2466556
