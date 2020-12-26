(defpackage #:day12
  (:use #:cl)
  (:import-from #:dop #:fn #:kw #:>>)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:-> #:->> #:% #:%%  #:2nd #:xor #:it #:=> #:when-it #:if-it
                #:ht->alist #:ht->plist #:ht-vals #:ht-keys #:dotable
                #:fmt #:length=)
  (:import-from #:alexandria #:when-let #:when-let* #:symbolicate)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day12)

(named-readtables:in-readtable :aoc)

(defparameter +input+ (aoc:puzzle 12 2020))

(defun parse-directions (input)
  (->> input str:lines (mapcar #`(cons (elt % 0) (parse-integer (subseq % 1))))))

(defparameter +directions+
  (parse-directions +input+))

(defparameter +example+
  (parse-directions "F10
N3
F7
R90
F11"))

(defun solve (directions)
  (let ((deltas '((#\E 1 0) (#\S 0 -1) (#\W -1 0) (#\N 0 1)))
        (x 0)
        (y 0)
        (direction 0))
    (loop for (action . value) in directions do
      (case action

        ((#\N #\S #\W #\E)
         (destructuring-bind (_ dx dy) (find-if #`(eq (car %) action) deltas)
           (declare (ignore _))
           (incf x (* dx value))
           (incf y (* dy value))))

        (#\F
         (destructuring-bind (_ dx dy) (elt deltas direction)
           (declare (ignore _))
           (incf x (* dx value))
           (incf y (* dy value))))

        (#\L (setf direction
                   (mod (- direction (floor value 90)) 4)))
        (#\R (setf direction
                   (mod (+ direction (floor value 90)) 4)))))
    (+ (abs x) (abs y))))

(solve +directions+) ;; 1148

(defparameter +deltas+
  '((#\E 1 0) (#\S 0 -1) (#\W -1 0) (#\N 0 1)))

(defun solve (directions)
  (let ((x 0) (y 0) (wx 10) (wy 1))
    (loop for (action . value) in directions do
      (case action

        ((#\N #\S #\W #\E)
         (destructuring-bind (_ dx dy) (find-if #`(eq (car %) action) +deltas+)
           (declare (ignore _))
           (incf wx (* dx value))
           (incf wy (* dy value))))

        (#\F
         (incf x (* wx value))
         (incf y (* wy value)))

        (#\L
         (loop repeat (floor value 90) do
           (psetf wx (- wy) wy wx)))

        (#\R
         (loop repeat (floor value 90) do
           (psetf wx wy wy (- wx))))))

    (+ (abs x) (abs y))))

(solve +directions+) ;; 52203
