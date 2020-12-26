(defpackage #:day24
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry))

(in-package #:day24)

(named-readtables:in-readtable :aoc)

(defun line->steps (line)
  (mapcar #`(coerce % 'string)
          (dop:group-by line :test #`(position % "ns"))))

(defun move (line &optional (x 0) (y 0))
  (let ((steps (line->steps line)))
    (loop for step in steps do
      (switch (step :test #'equal)
        ("w" (decf x))
        ("e" (incf x))
        ("nw" (decf y) (decf x))
        ("ne" (decf y))
        ("sw" (incf y))
        ("se" (incf y) (incf x))))
    (list x y)))

(defparameter *example*
  "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(defun init-floor (input)
  (let ((m (make-hash-table :test 'equal)))
    (loop for line in (str:lines input) do
      (let ((coords (move line)))
        (getsethash coords m 0)
        (incf (gethash coords m))))
    (maphash #`(if (evenp %%)
                   (remhash % m)
                   (setf (gethash % m) t))
             m)
    m))

(defun solve1 (input)
  (ht-count (init-floor input)))

(solve1 (dop:puzzle 24 2020)) ;; 360

(defun neighbours (x y)
  (mapcar #`(move % x y) (line->steps "wenenwsesw")))

(defun affected-tiles (floor)
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (loop for pos in (apply #'neighbours k) do
                 (setf (gethash pos result) t)))
             floor)
    (ht-keys result)))

(defun count-neighbours (floor x y)
  (loop for (nx ny) in (neighbours x y)
        count (gethash (list nx ny) floor)))

(defun solve2 (input &optional (cycles 100))
  (let ((current (init-floor input)))
    (loop repeat cycles do
      (let ((next (make-hash-table :test 'equal)))
        (loop for pos in (affected-tiles current) do
          (let ((count (apply #'count-neighbours current pos)))
            (if (gethash pos current)
                (when (< 0 count 3)
                  (setf (gethash pos next) t))
                (when (= 2 count)
                  (setf (gethash pos next) t)))))
        (setf current next)))
    (ht-count current)))

(solve2 (dop:puzzle 24 2020)) ;; 3924
