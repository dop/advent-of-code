(defpackage #:day20
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:define-constant))

(in-package #:day20)

(defparameter *example*
  "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(defmacro while-it (expr &body body)
  `(loop for it = ,expr while it do ,@body))

(defun take-while (pred seq)
  (do ((i 0 (1+ i)))
      ((or (= i (length seq))
           (not (funcall pred (elt seq i))))
       (values (subseq seq 0 i) (subseq seq i)))))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defun scan (preds seq)
  (let ((rez))
    (loop for pred in preds do
      (multiple-value-bind (match rest) (take-while pred (nth-value 1 (take-until pred seq)))
        (push match rez)
        (setf seq rest)))
    (nreverse rez)))

(defclass tile ()
  ((id
    :type number
    :initarg :id
    :accessor tile-id)
   (rows
    :type (vector string 10)
    :accessor tile-rows
    :initarg :rows)
   (top :type tile :accessor tile-top :initform nil :initarg :top)
   (bottom :type tile :accessor tile-bottom :initform nil :initarg :bottom)
   (left :type tile :accessor tile-left :initform nil :initarg :left)
   (right :type tile :accessor tile-right :initform nil :initarg :right)))

(defmethod print-object ((o tile) out)
  (print-unreadable-object (o out :type t)
    (format out "~<~:_id: ~a, top: ~a, right: ~a, bottom: ~a, left: ~a ~:@_rows: ~{~:i~a~^~:@_~} ~:>"
            (list
             (tile-id o)
             (aand (tile-top o) (tile-id it))
             (aand (tile-right o) (tile-id it))
             (aand (tile-bottom o) (tile-id it))
             (aand (tile-left o) (tile-id it))
             (coerce (tile-rows o) 'list)))))

(defun parse-tile (input)
  (destructuring-bind (id-line . rows) (str:lines input)
    (make-instance 'tile
                   :id (parse-integer (subseq id-line 5) :junk-allowed t)
                   :rows (coerce rows 'vector))))

(defun parse-input (input)
  (->> (cl-ppcre:split #?"\n\n" input)
       (mapcar #'parse-tile)))

(defun copy-tile (tile)
  (make-instance 'tile :id (tile-id tile)
                       :rows (tile-rows tile)
                       :top (tile-top tile)
                       :bottom (tile-bottom tile)
                       :left (tile-left tile)
                       :right (tile-right tile)))

(defmacro with-tile-copy ((copy tile) &body body)
  `(let ((,copy (copy-tile ,tile)))
     ,@body
     ,copy))

(defun flipped-tile (tile)
  (with-tile-copy (copy tile)
    (setf (tile-rows copy) (map 'vector #'reverse (tile-rows tile))
          (tile-right copy) (tile-left tile)
          (tile-left copy) (tile-right tile))))

(defun rotate-rows (rows)
  (let* ((len (length rows))
         (rotated (map 'vector #'copy-seq rows)))
    (loop for r from 0 below len do
      (loop for c from 0 below len do
        (setf (elt (elt rotated c) (- len r 1)) (elt (elt rows r) c))))
    rotated))

(defun rotated-tile (tile)
  (with-tile-copy (copy tile)
    (setf (tile-rows copy) (rotate-rows (tile-rows tile))
          (tile-right copy) (tile-top tile)
          (tile-bottom copy) (tile-right tile)
          (tile-left copy) (tile-bottom tile)
          (tile-top copy) (tile-left tile))))

(defun rotations (tile)
  (loop repeat 4
        for current = tile then (rotated-tile current)
        collect current))

(defun tile-top-edge (tile)
  (elt (tile-rows tile) 0))

(defun tile-bottom-edge (tile)
  (let ((rows (tile-rows tile)))
    (elt rows (1- (length rows)))))

(defun tile-right-edge (tile)
  (let* ((rows (tile-rows tile))
         (len (length rows)))
    (coerce (loop for i from 0 below len
                  collect (elt (elt rows i) (1- len)))
            'string)))

(defun tile-left-edge (tile)
  (let* ((rows (tile-rows tile))
         (len (length rows)))
    (coerce (loop for i from 0 below len
                  collect (elt (elt rows i) 0))
            'string)))

(defun matchp (tile1 tile2)
  (cond ((string= (tile-top-edge tile1) (tile-bottom-edge tile2))
         'top)
        ((string= (tile-top-edge tile2) (tile-bottom-edge tile1))
         'bottom)
        ((string= (tile-left-edge tile1) (tile-right-edge tile2))
         'left)
        ((string= (tile-left-edge tile2) (tile-right-edge tile1))
         'right)))

(defun versions (tile)
  (flat-map #'rotations (list tile (flipped-tile tile))))

(defun tile-neighbours (tile)
  (list (tile-top tile)
        (tile-right tile)
        (tile-bottom tile)
        (tile-left tile)))

(defmacro do-hash-table ((hash-table key value) &body body)
  (when body
    `(loop for ,key being the hash-keys of ,hash-table using (hash-value ,value)
           do ,@body)))

(defun connectedp (tile)
  (or (tile-top tile)
      (tile-bottom tile)
      (tile-left tile)
      (tile-right tile)))

(defun opposite (side)
  (ecase side
    (left 'right)
    (right 'left)
    (top 'bottom)
    (bottom 'top)))

(defun match-tiles (tiles)
  (let ((matched (make-hash-table)))
    ;; Put any tile as matched. Since there is unique correct matching
    ;; including all items, it does not matter where we start.
    (let* ((tile (pop tiles))
           (id (tile-id tile)))
      (setf (gethash id matched) tile))

    ;; First, we exhaust all unmatched tiles.
    (loop with unmatched and found while tiles do
      (do-hash-table (matched id tile)
        (loop for candidate = (pop tiles) while candidate do
          (unless (= id (tile-id candidate))
            (loop named search for version in (versions candidate)
                  do (when-it (matchp tile version)
                       (setf (slot-value tile it) version
                             (slot-value version (opposite it)) tile)
                       (push version found)
                       ;; (setf (gethash (tile-id version) matched) version)
                       (return-from search))
                  finally
                     (push candidate unmatched))))
        (setf tiles unmatched unmatched nil))
      (mapc #`(setf (gethash (tile-id %) matched) %) found)
      (setf found nil))

    ;; Then we complete matching of all sides.
    (do-hash-table (matched id1 tile1)
      (do-hash-table (matched id2 tile2)
        (unless (= id1 id2)
          (when-it (matchp tile1 tile2)
            (setf (slot-value tile1 it) tile2
                  (slot-value tile2 (opposite it)) tile1)))))

    matched))

(defun solve1 (input)
  (let ((matched (match-tiles (parse-input input))))
    (labels ((neighbours (tile)
               (list (tile-top tile) (tile-right tile) (tile-bottom tile) (tile-left tile))))
      (apply #'* (mapcar #'tile-id (keep-if #`(= 2 (count nil (neighbours %))) (ht-vals matched)))))))

;; (solve *example*)

;; (solve *example*) ;; 20899048083289

;; r0 r1 r2 r3 f0 f1 f2 f3
;;  0  1  2  3  4  5  6  7

(defun print-grid (2d)
  (loop for r from 0 below (array-dimension 2d 0) do
    (loop for c from 0 below (array-dimension 2d 1) do
      (princ (aref 2d r c)))
    (terpri)))

(defun rotate-grid (src)
  (let* ((h (array-dimension src 0))
         (w (array-dimension src 1))
         (dest (make-array (list w h) :element-type (array-element-type src))))
    (loop for r below h do
      (loop for c below w do
        (setf (aref dest c (- h r 1)) (aref src r c))))
    dest))

(defun flip-grid (src)
  (let* ((h (array-dimension src 0))
         (w (array-dimension src 1))
         (dest (make-array (list h w) :element-type (array-element-type src))))
    (loop for r below h do
      (loop for c below w do
        (setf (aref dest r (- w c 1)) (aref src r c))))
    dest))

(defun monster-rotations (monster)
  (loop repeat 4
        for current = monster then (rotate-grid current)
        collect current))

(defun monster-versions (monster)
  (flat-map #'monster-rotations (list monster (flip-grid monster))))

(defparameter *monster*
  (make-array '(3 20)
              :element-type 'character
              :displaced-to (with-output-to-string (out)
                              (write-string "                  # " out)
                              (write-string "#    ##    ##    ###" out)
                              (write-string " #  #  #  #  #  #   " out))))

(defun solve2 (input)
  (let* ((tiles (parse-input input))
         (tile-size (length (tile-rows (car tiles))))
         (grid-size (floor (sqrt (length tiles))))
         (matched (match-tiles tiles))
         (map-tile-size (- tile-size 2))
         (map-size (* map-tile-size grid-size))
         (map (make-array (list map-size map-size) :element-type 'character))
         (tile (find-if #`(not (or (tile-left %) (tile-top %))) (ht-vals matched))))
    (loop for r from 0
          for rp = tile then (tile-bottom rp) while rp
          do (loop for c from 0 for cp = rp then (tile-right cp) while cp do
            (loop for j from 0 below map-tile-size do
              (loop for i from 0 below map-tile-size do
                (setf (aref map (+ j (* r map-tile-size)) (+ i (* c map-tile-size)))
                      (elt (elt (tile-rows cp) (1+ j)) (1+ i)))))))
    ;; (print-grid map)

    (loop for monster in (monster-versions *monster*) do
      (let ((pos nil)
            (mh (array-dimension monster 0))
            (mw (array-dimension monster 1)))
        (loop for r below (1+ (- map-size mh)) do
          (loop for c below (1+ (- map-size mw)) do
            (loop named search
                  for j below mh
                  do (loop for i below mw
                           do (unless (or (and (eq #\# (aref map (+ r j) (+ c i)))
                                               (eq #\# (aref monster j i)))
                                          (eq #\space (aref monster j i)))
                                (return-from search)))
                  finally
                     (push (list r c) pos)
                     (loop for j below mh do
                       (loop for i below mw do
                         (when (eq #\# (aref monster j i))
                           (setf (aref map (+ r j) (+ c i)) #\O)))))))
        (when pos
          (print-grid map)
          (return-from solve2
            (loop for r below map-size
                  sum (loop for c below map-size
                            count (eq #\# (aref map r c))))))))))
