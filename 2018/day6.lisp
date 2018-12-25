(load "~/quicklisp/setup.lisp")
(ql:quickload :rutils)
(defpackage #:aoc-2018-day6 (:use #:cl #:rutils))
(in-package #:aoc-2018-day6)

(defconstant *coordinates*
  '((315 342)
    (59 106)
    (44 207)
    (52 81)
    (139 207)
    (93 135)
    (152 187)
    (271 47)
    (223 342)
    (50 255)
    (332 68)
    (322 64)
    (250 72)
    (165 209)
    (129 350)
    (139 118)
    (282 129)
    (311 264)
    (216 246)
    (134 42)
    (66 151)
    (263 199)
    (222 169)
    (236 212)
    (320 178)
    (202 288)
    (273 190)
    (83 153)
    (88 156)
    (284 305)
    (131 90)
    (152 88)
    (358 346)
    (272 248)
    (317 122)
    (166 179)
    (301 307)
    (156 128)
    (261 290)
    (268 312)
    (89 53)
    (324 173)
    (353 177)
    (91 69)
    (303 164)
    (40 221)
    (146 344)
    (61 314)
    (319 224)
    (98 143)))

(defparameter *coords*
  (mapindex (lambda (i x) (cons (code-char (+ 65 i)) x)) *coordinates*))

(defconstant *example*
  '((A 1 1)
    (B 1 6)
    (C 8 3)
    (D 3 4)
    (E 5 5)
    (F 8 9)))

(defun distance (x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defun print-area (area)
  (let* ((dim (array-dimensions area))
         (width (car dim))
         (height (cadr dim)))
    (loop for x from 0 below width do
      (loop for y from 0 below height do
        (format t "~a" (aref area x y)))
      (format t "~%"))))

(defun day6-1 (coordinates width height)
  (let ((counts (make-hash-table :test 'eq))
        (infinites nil))
    (loop :for y :from 0 :below height :do
      (loop :for x :from 0 :below width :do
        (loop
          :with d1 := (max width height) :and d2 := (max width height)
          :and id1 :and id2
          :for (id i j) :in coordinates
          :do (let ((d (distance x y i j)))
                (when (<= d d1)
                  (setf d2 d1 id2 id1)
                  (setf d1 d id1 id)))
          :finally
             (unless (eql d1 d2)
               (if (or (zerop x) (= x (1- width))
                       (zerop y) (= y (1- height)))
                   (pushnew id1 infinites))
               (incf (gethash id1 counts 0))))))
    (loop :for count :being :the :hash-values :in counts :using (hash-key id)
          :collect (cons id count) :into xs
          :finally
             (let ((finites (remove-if (lambda (x) (member (car x) infinites)) xs)))
               (return (sort finites (lambda (a b) (> (cdr a) (cdr b)))))))))

(subseq (day6-1 *coords* 500 500) 0 3) ;; => ((#\M . 4290) (#\Z . 3943) (#\E . 3855))

(defun day6-2 (coordinates width height)
  (let ((count 0))
    (loop :for y :from 0 :below height :do
      (loop :for x :from 0 :below width :do
        (when (< (apply #'+ (loop :for (i j) :in coordinates :collect (distance x y i j)))
               10000)
          (incf count))))
    count))

(day6-2 *coordinates* 500 500) ;; => 37318
