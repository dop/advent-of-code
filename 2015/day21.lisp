(defparameter *boss*
  '(:life 109 :damage 8 :armor 2))

(defparameter *me*
  '(:life 100 :damage 0 :armor 0))

(defparameter *weapons*
  '((dagger :cost 8 :damage 4 :armor 0)
    (shortsword :cost 10 :damage 5 :armor 0)
    (warhammer :cost 25 :damage 6 :armor 0)
    (longsword :cost 40 :damage 7 :armor 0)
    (greataxe :cost 74 :damage 8 :armor 0)))

(defparameter *armor*
  '((_ :cost 0 :damage 0 :armor 0)
    (leather :cost 13 :damage 0 :armor 1)
    (chainmail :cost 31 :damage 0 :armor 2)
    (splintmail :cost 53 :damage 0 :armor 3)
    (bandedmail :cost 75 :damage 0 :armor 4)
    (platemail :cost 102 :damage 0 :armor 5)))

(defparameter *rings*
  '((damage+1 :cost 25 :damage 1 :armor 0)
    (damage+2 :cost 50 :damage 2 :armor 0)
    (damage+3 :cost 100 :damage 3 :armor 0)
    (defense+1 :cost 20 :damage 0 :armor 1)
    (defense+2 :cost 40 :damage 0 :armor 2)
    (defense+3 :cost 80 :damage 0 :armor 3)))

(defun play ()
  (let ((most-gold 0))
    (loop for (_ . weapon) in *weapons* do
      (loop for (_ . armor) in *armor* do
        (loop for (_ . ring1) in *rings* do
          (loop for (_ . ring2) in *rings* when (not (eq ring1 ring2)) do
            (setf (getf *me* :life) 100
                  (getf *me* :damage) (+ (getf weapon :damage) (getf armor :damage) (getf ring1 :damage) (getf ring2 :damage))
                  (getf *me* :armor) (+ (getf weapon :armor) (getf armor :armor) (getf ring1 :armor) (getf ring2 :armor)))
            (setf (getf *boss* :life) 109
                  (getf *boss* :damage) 8
                  (getf *boss* :armor) 2)
            (when (> (/ (getf *boss* :life) (max 1 (- (getf *me* :damage) (getf *boss* :armor))))
                     (/ (getf *me* :life) (max 1 (- (getf *boss* :damage) (getf *me* :armor)))))
              (setf most-gold (max most-gold (+ (getf weapon :cost) (getf armor :cost) (getf ring1 :cost) (getf ring2 :cost)))))))))
    most-gold))
