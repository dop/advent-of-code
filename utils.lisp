(defpackage #:dop
  (:use #:cl #:should-test)
  (:import-from #:alexandria
                #:map-product
                #:read-file-into-string
                #:write-string-into-file
                #:curry)
  (:import-from #:named-readtables #:in-readtable)
  (:import-from #:trivia
                #:defpattern
                #:match
                #:ematch)
  (:import-from #:rutils
                #:it
                #:->
                #:->>
                #:%)
  (:export #:rotate
           #:binary-split
           #:translate #:translate!
           #:incfhash
           #:doseq
           #:with-collecting #:counting #:summing #:maximizing #:minimizing #:collecting
           #:kw
           #:neighbours
           #:group-by))

(in-package #:dop)

(in-readtable :aoc)

(defun kw (&rest strings)
  (intern (str:join #\- (mapcar #'string-upcase strings)) 'keyword))

(defun rotate (seq &optional (n 1))
  "Safe rotate. Does not modify original sequence. Takes items from beginning of
sequence and pushes to the back. If N is negative, takes items from the end and
puts them in the front."
  (when seq
    (let ((l (length seq)))
      (if (or (zerop n) (zerop l))
          seq
          (let ((k (rem n l)))
            (cond
              ((zerop k) seq)
              ((plusp k)
               (concatenate (type-of seq)
                            (subseq seq k)
                            (subseq seq 0 k)))
              ((minusp k)
               (concatenate (type-of seq)
                            (subseq seq (+ l k))
                            (subseq seq 0 (+ l k))))))))))

(deftest test-rotate ()
  (should be equal '(2 3 1) (rotate '(1 2 3)))
  (should be equal nil (rotate nil))
  (should be equal '(3 1 2) (rotate '(1 2 3) 2))
  (should be equal '(1 2 3) (rotate '(1 2 3) 3))
  (should be equal '(2 3 1) (rotate '(1 2 3) 4))

  (should be equal '(3 1 2) (rotate '(1 2 3) -1))
  (should be equal '(2 3 1) (rotate '(1 2 3) -2))
  (should be equal '(1 2 3) (rotate '(1 2 3) -3))
  (should be equal '(3 1 2) (rotate '(1 2 3) -4))

  (should be equalp #(2 3 1) (rotate #(1 2 3)))
  (should be equalp #() (rotate #()))
  (should be equalp #(3 1 2) (rotate #(1 2 3) 2))
  (should be equalp #(1 2 3) (rotate #(1 2 3) 3))
  (should be equalp #(2 3 1) (rotate #(1 2 3) 4))

  (should be equalp #(3 1 2) (rotate #(1 2 3) -1))
  (should be equalp #(2 3 1) (rotate #(1 2 3) -2))
  (should be equalp #(1 2 3) (rotate #(1 2 3) -3))
  (should be equalp #(3 1 2) (rotate #(1 2 3) -4))

  (should be equalp "231" (rotate "123"))
  (should be equalp "" (rotate ""))
  (should be equalp "312" (rotate "123" 2))
  (should be equalp "123" (rotate "123" 3))
  (should be equalp "231" (rotate "123" 4))

  (should be equalp "312" (rotate "123" -1))
  (should be equalp "231" (rotate "123" -2))
  (should be equalp "123" (rotate "123" -3))
  (should be equalp "312" (rotate "123" -4)))

(test :test 'test-rotate)

(defun first-key (dimensions)
  (make-list (length dimensions) :initial-element 0))

(defun next-key (current-key dimensions)
  (let ((next-key (copy-seq current-key)))
    (loop :for i :from (1- (length dimensions)) :downto 0
          :do (if (< (elt current-key i) (1- (elt dimensions i)))
                  (progn
                    (incf (elt next-key i))
                    (return next-key))
                  (setf (elt next-key i) 0)))))


(defun binary-split (x y)
  (let ((m (+ x (floor (- y x) 2))))
    (list x m (1+ m) y)))

(defun translate! (dictionary sequence)
  (loop for (k v) in dictionary do (nsubstitute v k sequence))
  sequence)

(defun translate (dictionary sequence)
  (loop for (k v) in dictionary
        for id = (substitute v k sequence) then (substitute v k id)
        finally (return id)))

(defmethod print-object ((o hash-table) stream)
  (princ "{" stream)
  (let ((first t))
    (maphash (lambda (k v)
               (if first
                   (rtl:void first)
                   (write-char #\space stream))
               (format stream " ~S ~S~%" k v))
             o))
  (princ "}" stream))

(defpattern hash-key (key pattern)
  (alexandria:with-gensyms (it)
    `(trivia:guard1 ,it (nth-value 1 (gethash ,key ,it))
                    (gethash ,key ,it) ,pattern)))

(defpattern hash-table (&rest kvs)
  `(and (type hash-table)
        ,@(loop for (key pattern) on kvs by #'cddr
                collect `(hash-key ,key ,pattern))))

(defun incfhash (key table &key (by 1) (start 0))
  (symbol-macrolet ((value (gethash key table)))
    (unless (nth-value 1 value)
      (setf value start))
    (incf value by)))

(defun neighbours (pos)
  "Returns all neighbours of any-dimensional POS defined as list of
integer coordinates."
  (when pos
    (let (result)
      (apply #'map-product (lambda (&rest delta)
                             (unless (apply #'= 0 delta)
                               (push (mapcar #'+ pos delta) result)))
             (loop repeat (length pos) collect '(-1 0 1)))
      result)))

(defun group-by (sequence &key (test #'=) (key #'identity))
  (when (length sequence)
    (->> (reduce (lambda (acc x)
                   (if (funcall test
                                (funcall key (caar acc))
                                (funcall key x))
                       (cons (cons x (car acc)) (cdr acc))
                       (cons (list x) acc)))
                 (subseq sequence 1)
                 :initial-value (list (list (elt sequence 0))))
         (mapcar #'nreverse)
         nreverse)))
