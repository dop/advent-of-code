(defpackage #:day11
  (:use #:cl)
  (:import-from #:dop #:fn #:kw #:>>)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:-> #:->> #:% #:%%  #:2nd #:xor #:it #:=> #:when-it #:if-it
                #:ht->alist #:ht->plist #:ht-vals #:ht-keys #:dotable
                #:fmt #:length=)
  (:import-from #:alexandria #:when-let #:when-let* #:symbolicate)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day11)

(named-readtables:in-readtable :aoc)

(defparameter +example+
  "L.LL.LL.LL
  LLLLLLL.LL
  L.L.L..L..
  LLLL.LL.LL
  L.LL.LL.LL
  L.LLLLL.LL
  ..L.L.....
  LLLLLLLLLL
  L.LLLLLL.L
  L.LLLLL.LL")

(defgeneric iterator-of (o))

(defmethod iterator-of ((o array)))

(iterator +example-grid+)

(defun to-grid (input)
  (->> input
       str:lines
       (mapcar #'str:trim)
       (make-array (list (length %) (length (car %))) :initial-contents %)))

(defun cell (i j arr)
  (when (and (< -1 i (array-dimension arr 1))
             (< -1 j (array-dimension arr 0)))
    (aref arr j i)))

(defun (setf cell) (newval i j arr)
  (when (cell i j arr)
    (setf (aref arr j i) newval)))

(defun neighbour-indexes (i j arr)
  (let (result)
    (destructuring-bind (max-j max-i) (array-dimensions arr)
      (loop for di from -1 to 1 do
        (loop for dj from -1 to 1 do
          (when (not (= di dj 0))
            (let ((ni (+ i di))
                  (nj (+ j dj)))
              (when (and (< -1 ni max-i)
                         (< -1 nj max-j))
                (push (cons ni nj) result))))))
      result)))

(defun neighbours (i j arr)
  (loop for (ni . nj) in (neighbour-indexes i j arr)
        collect (cell ni nj arr)))

(defparameter +example-grid+
  (to-grid +example+))

(defun simple-next-cell-state (i j arr)
  (case (cell i j arr)
    (#\. #\.)
    (#\# (if (< (count #\# (neighbours i j arr)) 4)
             #\#
             #\L))
    (#\L (if (= 0 (count #\# (neighbours i j arr)))
             #\#
             #\L))))

(defun write-next-grid-state (next-cell src dest)
  (let ((changes 0))
    (loop for i from 0 below (array-dimension src 1) do
      (loop for j from 0 below (array-dimension src 0) do
        (let ((curr (cell i j src))
              (next (funcall next-cell i j src)))
          (setf (cell i j dest) next)
          (incf changes (if (eq curr next) 0 1)))))
    changes))

(defun print-grid (arr)
  (loop for i from 0 below (array-dimension arr 1) do
    (loop for j from 0 below (array-dimension arr 0) do
      (princ (cell i j arr)))
    (terpri)))

(defun solve (input next-cell &optional (limit 1000))
  (let* ((grid (to-grid input))
         (next (make-array (array-dimensions grid) :initial-element #\.)))
    (loop named inner for i from 1 to limit do
      (when (= i limit)
        (error "Limit reached."))
      (when (zerop (write-next-grid-state next-cell grid next))
        (return-from inner))
      (rotatef grid next))
    (let ((count 0))
      (loop for i from 0 below (array-dimension grid 1) do
        (loop for j from 0 below (array-dimension grid 0) do
          (incf count (if (eq #\# (cell i j grid)) 1 0))))
      count)))

(solve (aoc:puzzle 11 2020) #'simple-next-cell-state) ;; 2113

(defmacro with-dimensions ((arr &rest subscripts) &body body)
  `(destructuring-bind ,subscripts (array-dimensions ,arr)
     ,@body))

(with-dimensions (+example-grid+ r c)
  (loop for j below r do
    (loop for i below c do
      (princ (aref +example-grid+ j i)))
    (terpri)))

(defun visible-seats (i j arr)
  (let ((result))
    (loop for (dx . dy) in (loop for (x . y) in (neighbour-indexes i j arr)
                                 collect (cons (- x i) (- y j)))
          do (loop named inner
                   for ni = (+ i dx) then (+ ni dx)
                   and nj = (+ j dy) then (+ nj dy)
                   for cell = (cell ni nj arr)
                   while cell
                   if (member cell '(#\L #\#))
                     do (push cell result)
                        (return-from inner)))
    result))

(defun complex-next-cell-state (i j arr)
  (case (cell i j arr)
    (#\. #\.)
    (#\# (if (< (count #\# (visible-seats i j arr)) 5)
             #\#
             #\L))
    (#\L (if (= 0 (count #\# (visible-seats i j arr)))
             #\#
             #\L))))

(defparameter +input+ (to-grid (aoc:puzzle 11 2020)))

(solve (aoc:puzzle 11 2020) #'complex-next-cell-state) ;; 1865
