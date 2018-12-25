(ql:quickload '(:rutils :optima))
(cl:defpackage #:aoc-2018-day15
  (:use #:cl #:optima #:rutils)
  (:import-from #:alexandria #:curry #:compose))
(cl:in-package #:aoc-2018-day15)

(defparameter *real-world*
  (string-trim '(#\newline) (rutils:slurp "day15.txt")))

(defparameter *frustrating-world*
  (string-trim '(#\newline) (rutils:slurp "day15-frustrating.txt")))

(defparameter *example*
  "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")

(defparameter *elf-attack-power* 3)

(deftype creature ()
  '(member elf goblin))

(defstruct unit
  (race 'elf :type creature)
  (life 200)
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun attack-power (unit)
  (case (unit-race unit)
    (elf *elf-attack-power*)
    (t 3)))

(defun read-world (world)
  (let ((walls (make-hash-table :test 'equal))
        (units nil))
    (loop
      :with y := 0 :and x := 0
      :for tile :across (string-trim '(#\newline) world)
      :do
         (ecase tile
           (#\#
            (setf (gethash (cons x y) walls) t))
           (#\. nil)
           (#\E (push (make-unit :race 'elf :x x :y y) units))
           (#\G (push (make-unit :race 'goblin :x x :y y) units))
           (#\newline
            (setf x -1)
            (incf y)))
         (incf x)
      :maximize x :into width
      :maximize y :into height
      :finally (return (list :cycle 0
                             :width width
                             :height (1+ height)
                             :units units
                             :walls walls)))))

(defun unit-xy (unit)
  (cons (unit-x unit)
        (unit-y unit)))

(defun unit-char (unit)
  (ecase (unit-race unit)
    (goblin #\G)
    (elf #\E)))

(defun to-character (x y walls units)
  (if (gethash (cons x y) walls)
      #\#
      (let ((unit (find (cons x y) units :test #'equal :key #'unit-xy)))
        (if unit
            (unit-char unit)
            #\space))))

(defun print-world (world)
  (let ((width (getf world :width))
        (height (getf world :height))
        (units (getf world :units))
        (walls (getf world :walls)))
    (with-output-to-string (out)
      (loop :for y :from 0 :below height :do
        (loop :for x :from 0 :below width :do
          (write-char (to-character x y walls units) out))
        (write-char #\newline out)))))

(defun units-in-order (units)
  (sort (copy-list units)
        #'by-coordinates))

(defun xy-less-p (xy1 xy2)
  (let ((x1 (car xy1))
        (y1 (cdr xy1))
        (x2 (car xy2))
        (y2 (cdr xy2)))
    (or (< y1 y2) (and (= y1 y2) (< x1 x2)))))

(defun by-coordinates (a b)
  (xy-less-p (unit-xy a) (unit-xy b)))

(defun is-adjacent-p (a b)
  (= (+ (abs (- (unit-x a) (unit-x b)))
         (abs (- (unit-y a) (unit-y b))))
     1))

(defun friend-p (a b)
  (eql (unit-race a) (unit-race b)))

(defun enemy-p (a b)
  (not (friend-p a b)))

(defun can-attack-p (a b)
  (and (enemy-p a b) (is-adjacent-p a b)))

(defun by-health (a b)
  (< (unit-life a) (unit-life b)))

(defun elf-p (unit)
  (eql (unit-race unit) 'elf))

(defun goblin-p (unit)
  (eql (unit-race unit) 'goblin))

(defun outcome (world)
  (* (getf world :cycle)
     (apply #'+ (mapcar #'unit-life (getf world :units)))))

(defun find-victim (attacker units)
  (car (sort (remove-if-not (curry #'can-attack-p attacker) units)
             (lambda (a b)
               (or (by-health a b)
                   (and (= (unit-life a) (unit-life b))
                        (by-coordinates a b)))))))

(defun attack (unit &optional (power 3))
  (decf (unit-life unit) power))

(defun obstacles (walls units)
  (let ((obstacles (copy-hash-table walls)))
    (loop :for unit :in units :do
      (setf (gethash (cons (unit-x unit) (unit-y unit)) obstacles) t))
    obstacles))

(defun find-target (attacker units walls)
  (loop
    :with closest-enemy :and closest-distance :and dmap
    :for enemy :in (remove-if-not (curry #'enemy-p attacker) units)
    :do
       (let* ((distance-map (distance-map (unit-x enemy) (unit-y enemy)
                                          (obstacles walls (remove attacker units))))
              (distance (gethash (unit-xy attacker) distance-map)))
         (when (and distance
                    (or (not closest-distance)
                        (> closest-distance distance)
                        (and (= closest-distance distance)
                             (by-coordinates enemy closest-enemy))))
           (setf closest-enemy enemy
                 closest-distance distance
                 dmap distance-map)))
    :finally
       (when closest-enemy
         (return (list closest-enemy dmap)))))

(defun next-step (attacker distance-map)
  (loop
    :with nx :and ny :and distance
    :for xy :in (adjacent-xy (unit-x attacker) (unit-y attacker)) :do
      (when-it (gethash xy distance-map)
        (when (or (not distance)
                  (< it distance)
                  (and (= it distance)
                       (xy-less-p xy (cons nx ny))))
          (setf nx (car xy)
                ny (cdr xy)
                distance it)))
    :finally
       (when (and nx ny)
         (return (cons nx ny)))))

(defun alive-p (unit)
  (plusp (unit-life unit)))

(defun move (units walls)
  (loop
    :with sorted-units := (units-in-order units)
    :with current-units := units
    :for unit :in sorted-units
    :do
       (when (alive-p unit)
         (let ((power (attack-power unit)))
           (if-it (find-victim unit current-units)
                  (attack it power)
                  (when-it (find-target unit current-units walls)
                    (let ((distance-map (second it)))
                      (when-it (next-step unit distance-map)
                        (setf (unit-x unit) (car it)
                              (unit-y unit) (cdr it))
                        (when-it (find-victim unit current-units)
                          (attack it power))))))
           (setf current-units (remove-if-not #'alive-p current-units))))
    :finally
       (return current-units)))

(defun go-round (world)
  (let ((units (move (getf world :units) (getf world :walls))))
    (setf (getf world :units) units)
    (unless (or (notany #'elf-p (getf world :units))
                (notany #'goblin-p (getf world :units)))
      (incf (getf world :cycle))))
  world)

(defun adjacent-xy (x y)
  `((,(1+ x) . ,y) (,(1- x) . ,y) (,x . ,(1+ y)) (,x . ,(1- y))))

(defun distance-map (x y obstacles)
  (let ((distances (make-hash-table :test 'equal)))
    (setf (gethash (cons x y) distances) 0)
    (flet ((occupiedp (xy)
             (or (gethash xy distances)
                 (gethash xy obstacles)))
           (nearby-values (xy)
             (remove nil (mapcar (lambda (xy) (gethash xy distances))
                                 (adjacent-xy (car xy) (cdr xy))))))
        (let ((queue (remove-if #'occupiedp (adjacent-xy x y))))
          (loop :until (null queue) :do
            (let ((xy (pop queue)))
              (unless (occupiedp xy)
                (let ((next-cells (remove-if #'occupiedp (adjacent-xy (car xy) (cdr xy)))))
                  (setf queue (append queue next-cells)
                        (gethash xy distances) (1+ (apply #'min (nearby-values xy))))))))))
    distances))

(defun print-distances (x y world)
  (let ((width (getf world :width))
        (height (getf world :height))
        (units (getf world :units))
        (walls (getf world :walls)))
    (let* ((obstacles (obstacles walls units))
           (distances (distance-map x y obstacles)))
        (with-output-to-string (out)
          (loop :for y :from 0 :below height :do
            (loop :for x :from 0 :below width :do
              (if (gethash (cons x y) obstacles)
                  (if (gethash (cons x y) walls)
                      (write-char #\# out)
                      (write-char #\? out))
                  (let ((distance (gethash (cons x y) distances)))
                    (if distance
                        (format out "~X" distance)
                        (write-char #\. out)))))
            (write-char #\newline out))))))

(defun fight (w)
  (loop :while (and (some #'elf-p (getf w :units))
                    (some #'goblin-p (getf w :units)))
        :do (go-round w)
        :finally (return (list :outcome (outcome w)
                               :world w))))

(defun cheat (world-definition)
  (let* ((world (read-world world-definition))
         (elf-count (count-if #'elf-p (getf world :units))))
    (loop :for power :from 4 :do
      (let ((world-copy (read-world world-definition))
            (*elf-attack-power* power))
        (let ((result (fight world-copy)))
          (when (= elf-count (count-if #'elf-p (getf world-copy :units)))
            (return (list power result))))))))
