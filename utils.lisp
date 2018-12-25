(ql:quickload '(#:should-test #:optima))

(defpackage #:dop
  (:use #:cl #:should-test #:optima)
  (:export #:each #:amap #:namap #:rotate #:namapi #:amapi))

(in-package #:dop)

(defun rotate (seq &optional (n 1))
  "Safe rotate. Does not modify original sequence. Takes items from beginning of
sequence and pushes to the back. If N is negative, takes items from the end and
puts then in the front."
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

(defun each (arr fn)
  (loop
    :with dimensions := (array-dimensions arr)
    :for current-key := (first-key dimensions) :then (next-key current-key dimensions)
    :while current-key
    :do (apply fn current-key)))

(let (result)
  (each (make-array '(2 3)) (lambda (x y) (push (cons x y) result)))
  (should be equal '((1 . 2) (1 . 1) (1 . 0) (0 . 2) (0 . 1) (0 . 0)) result))

(defun amapi (arr fn)
  (let ((arr2 (make-array (array-dimensions arr))))
    (each arr (lambda (&rest args)
                (setf (apply #'aref arr2 args) (apply fn args))))
    arr2))

(should be equalp
        #2A(((0 . 0) (0 . 1) (0 . 2)) ((1 . 0) (1 . 1) (1 . 2)))
        (amapi (make-array '(2 3)) (lambda (x y) (cons x y))))

(defun namapi (arr fn)
  (each arr (lambda (&rest args)
              (setf (apply #'aref arr args) (apply fn args))))
  arr)

(should be equalp
        #2A(((0 . 0) (0 . 1) (0 . 2)) ((1 . 0) (1 . 1) (1 . 2)))
        (namapi (make-array '(2 3)) (lambda (x y) (cons x y))))

(defun namap (arr fn)
  (each arr (lambda (&rest args)
              (setf (apply #'aref arr args)
                    (funcall fn (apply #'aref arr args)))))
  arr)

(should be equalp #2A((1 1 1) (1 1 1)) (namap (make-array '(2 3)) (lambda (v) (1+ v))))

(defun amap (arr fn)
  (amapi arr (lambda (&rest args)
               (setf (apply #'aref arr args)
                     (funcall fn (apply #'aref arr args))))))

(should be equalp #2A((1 1 1) (1 1 1)) (amap (make-array '(2 3)) (lambda (v) (1+ v))))
