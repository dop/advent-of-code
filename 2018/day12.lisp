(ql:quickload '(:rutils :dlist))
(cl:defpackage #:aoc-2018-day12 (:use #:cl))
(cl:in-package #:aoc-2018-day12)

(defparameter *pattern* "#..#.#..##......###...###")
(defparameter *state* (cons 0 *pattern*))

(defparameter *transitions*
  '(("...##" . #\#)
    ("..#.." . #\#)
    (".#..." . #\#)
    (".#.#." . #\#)
    (".#.##" . #\#)
    (".##.." . #\#)
    (".####" . #\#)
    ("#.#.#" . #\#)
    ("#.###" . #\#)
    ("##.#." . #\#)
    ("##.##" . #\#)
    ("###.." . #\#)
    ("###.#" . #\#)
    ("####." . #\#)))

(defun sum-plants (start-index plants)
  (loop :for i :from start-index
        :for c :across plants
        :if (char= #\# c) :sum i))

(= (sum-plants -2 "#....##....#####...#######....#.#..##.") 325)

(defun cleanup-pattern (pattern)
  (string-trim '(#\.) pattern))

(defun cleanup-state (state)
  (destructuring-bind (start-index . pattern) state
    (cons start-index (cleanup-pattern pattern))))

(defun pad-pattern (pattern)
  (concatenate 'string
               (make-string 4 :initial-element #\.)
               pattern
               (make-string 4 :initial-element #\.)))

(defun next-state (state transitions)
  (destructuring-bind (start-index . plants) state
    (loop
      :with padded-plants := (pad-pattern plants)
      :with seen-first-plant-at-index := nil
      :for i :from 0 :below (- (length padded-plants) 4)
      :for new-start-index :from (- start-index 2)
      :collect
      (loop
        :for (pattern . result) :in transitions
        :do
           (when (string= pattern padded-plants :start2 i :end2 (+ i 5))
             (when (and (not seen-first-plant-at-index) (char= result #\#))
               (setf seen-first-plant-at-index new-start-index))
             (return result))
        :finally
           (return #\.)) :into next-plants
      :finally
         (return (cleanup-state (cons seen-first-plant-at-index
                                      (coerce next-plants 'string)))))))


(defparameter *correct*
  #("...#..#.#..##......###...###..........."
    "...#...#....#.....#..#..#..#..........."
    "...##..##...##....#..#..#..##.........."
    "..#.#...#..#.#....#..#..#...#.........."
    "...#.#..#...#.#...#..#..##..##........."
    "....#...##...#.#..#..#...#...#........."
    "....##.#.#....#...#..##..##..##........"
    "...#..###.#...##..#...#...#...#........"
    "...#....##.#.#.#..##..##..##..##......."
    "...##..#..#####....#...#...#...#......."
    "..#.#..#...#.##....##..##..##..##......"
    "...#...##...#.#...#.#...#...#...#......"
    "...##.#.#....#.#...#.#..##..##..##....."
    "..#..###.#....#.#...#....#...#...#....."
    "..#....##.#....#.#..##...##..##..##...."
    "..##..#..#.#....#....#..#.#...#...#...."
    ".#.#..#...#.#...##...#...#.#..##..##..."
    "..#...##...#.#.#.#...##...#....#...#..."
    "..##.#.#....#####.#.#.#...##...##..##.."
    ".#..###.#..#.#.#######.#.#.#..#.#...#.."
    ".#....##....#####...#######....#.#..##."))

(loop
  :for i :from 0 :to 1000
  :for state := *state* :then (next-state state *transitions*)
  :do (when (zerop (rem i 100))
        (format t "~a ~a ~a~%" i state
                ;; (equal (cdr state) (cleanup-pattern (elt *correct* i)))
                (sum-plants (car state) (cdr state)))))


(defparameter *state*
  (cons 0 "####....#...######.###.#...##....#.###.#.###.......###.##..##........##..#.#.#..##.##...####.#..##.#"))

(defparameter *transitions*
  '(("..#.." . #\.)
    ("#.#.#" . #\#)
    ("#.###" . #\#)
    (".##.." . #\.)
    ("#.#.." . #\#)
    (".#.#." . #\#)
    (".###." . #\#)
    (".####" . #\#)
    ("##..." . #\#)
    ("#.##." . #\#)
    ("#..##" . #\#)
    ("....#" . #\.)
    ("###.#" . #\.)
    ("#####" . #\#)
    ("....." . #\.)
    ("..#.#" . #\.)
    (".#..." . #\#)
    ("##.#." . #\.)
    (".#.##" . #\#)
    ("..##." . #\.)
    ("#...#" . #\.)
    ("##.##" . #\#)
    ("...#." . #\.)
    ("#..#." . #\.)
    ("..###" . #\.)
    (".##.#" . #\.)
    ("#...." . #\.)
    (".#..#" . #\#)
    ("####." . #\.)
    ("...##" . #\#)
    ("##..#" . #\.)
    ("###.." . #\.)))

(loop
  :for i :from 0 :to 2000
  :for state := *state* :then (next-state state *transitions*)
  :do (when (zerop (rem i 100))
        (format t "~a ~a ~a~%" i state
                ;; (equal (cdr state) (cleanup-pattern (elt *correct* i)))
                (sum-plants (car state) (cdr state)))))

(+ (* 3200 (/ (- 50000000000 100) 100)) 3528) ;; => 1600000000328
