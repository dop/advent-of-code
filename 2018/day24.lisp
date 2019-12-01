(ql:quickload '(:rutils :optima :fset :iterate :priority-queue))

(cl:defpackage #:aoc201824
  (:use #:cl #:iterate #:priority-queue)
  (:import-from #:alexandria #:compose #:curry #:rcurry)
  (:import-from #:rutils #:when-it #:it))

(cl:in-package #:aoc201824)

(defstruct group
  (id 0)
  (type nil)
  (units 0 :type integer)
  (hit 0 :type integer)
  (weak nil)
  (immune nil)
  (initiative 0)
  (damage-type nil)
  (damage 0))

(defun effective-power (group)
  (let ((boost (if (good-p group) *immune-boost* 0)))
    (* (group-units group) (+ boost (group-damage group)))))

(defun effective-damage (attacker victim)
  (cond
    ((member (group-damage-type attacker) (group-immune victim)) 0)
    ((member (group-damage-type attacker) (group-weak victim)) (* 2 (effective-power attacker)))
    (t (effective-power attacker))))

(defun friend-p (a b)
  (eq (group-type a) (group-type b)))

(defun enemy-p (a b)
  (not (friend-p a b)))

(defun choose-victim (attacker groups)
  (let ((best-initiative 0)
        (best-damage 0)
        (best-effective-power 0)
        (best nil))
    (iterate (for victim in groups)
      (let ((damage (effective-damage attacker victim))
            (initiative (group-initiative victim))
            (effective-power (effective-power victim)))
        (when (and (enemy-p attacker victim)
                   (> damage 0)
                   (or (not best)
                       (> damage best-damage)
                       (and (= damage best-damage)
                            (or (> effective-power best-effective-power)
                                (and (= effective-power best-effective-power)
                                     (> initiative best-initiative))))))
          (setf best victim
                best-initiative initiative
                best-damage damage
                best-effective-power effective-power))))
    best))

(defun in-selection-order (groups)
  (sort (copy-seq groups)
        (lambda (a b)
          (let ((ap (effective-power a))
                (bp (effective-power b)))
            (or (> ap bp) (and (= ap bp)
                               (> (group-initiative a) (group-initiative b))))))))

(defun good-p (group)
  (eq 'good (group-type group)))

(defun bad-p (group)
  (eq 'bad (group-type group)))

(defun in-attack-order (alist)
  (sort (copy-seq alist) (lambda (pa pb)
                           (let ((a (car pa))
                                 (b (car pb)))
                             (> (group-initiative a) (group-initiative b))))))

(defun alive-p (g)
  (> (group-units g) 0))

(defun group->string (group)
  (format nil "<~a id:~a, units:~a>" (group-type group) (group-id group) (group-units group)))

(defun fight (groups)
  (loop :for i :from 1 :while (and (some #'good-p groups) (some #'bad-p groups)) :do
    ;; (format t "ITERATION ~a~%" i)
    (let ((pairs (fset:empty-map))
          (available groups)
          (total-kills 0))
      (iterate (for attacker in (in-selection-order groups))
        (when-it (choose-victim attacker available)
          ;; (format t "~a chose to attack ~a, damage ~a~%"
          ;;         (group->string attacker)
          ;;         (group->string it)
          ;;         (effective-damage attacker it))
          (setf available (remove it available)
                pairs (fset:with pairs attacker it))))
      (iterate (for (attacker . victim) in (in-attack-order (fset:convert 'list pairs)))
        (when (alive-p attacker)
          (let ((kills (truncate (effective-damage attacker victim) (group-hit victim))))
            ;; (format t "~a kills ~a of ~a~%"
            ;;         (group->string attacker)
            ;;         (min kills (group-units victim))
            ;;         (group->string victim))
            (incf total-kills kills)
            (decf (group-units victim) kills))))
      (when (zerop total-kills)
        (return-from fight nil))
      (setf groups (remove-if-not #'alive-p groups))))
  groups)

(defparameter *immune-boost* 0)

(let ((*immune-boost* 1570))
  (apply #'+ (mapcar #'group-units (remove-if #'bad-p (fight (mapcar #'copy-group *example*))))))

(loop :for boost :from 1 :do
  (let* ((*immune-boost* boost)
         (units (apply #'+ (mapcar #'group-units (remove-if #'bad-p (fight (mapcar #'copy-group *groups*)))))))
    (when (plusp units)
      (return (values boost units)))))

(defparameter *example*
  (list (make-group :id 1
                    :type 'good :units 17 :hit 5390 :weak '(radiation bludgeoning)
                    :damage 4507 :damage-type 'fire :initiative 2)
        (make-group :id 2
                    :type 'good :units 989 :hit 1274 :immune '(fire) :weak '(bludgeoning slashing)
                    :damage 25 :damage-type 'slashing :initiative 3)

        (make-group :id 1
                    :type 'bad :units 801 :hit 4706 :weak '(radiation)
                    :damage 116 :damage-type 'bludgeoning :initiative 1)
        (make-group :id 2
                    :type 'bad :units 4485 :hit 2961 :immune '(radiation) :weak '(fire cold)
                    :damage 12 :damage-type 'slashing :initiative 4)))



(defparameter *groups*
  (list (make-group :id 0 :type 'good :units 2334 :hit 8900 :damage 31 :damage-type 'fire :initiative 4)
        (make-group :id 1 :type 'good :units 411 :hit 8067 :damage 195 :damage-type 'radiation :initiative 1)
        (make-group :id 2 :type 'good :units 449 :hit 9820 :weak '(fire) :damage 193 :damage-type 'bludgeoning :initiative 3)
        (make-group :id 3 :type 'good :units 452 :hit 4418 :weak '(fire) :immune '(bludgeoning) :damage 89 :damage-type 'bludgeoning :initiative 10)
        (make-group :id 4 :type 'good :units 858 :hit 5016 :weak '(bludgeoning fire) :immune '(slashing) :damage 58 :damage-type 'bludgeoning :initiative 18)
        (make-group :id 5 :type 'good :units 3049 :hit 9940 :damage 29 :damage-type 'cold :initiative 12)
        (make-group :id 6 :type 'good :units 610 :hit 7021 :weak '(bludgeoning radiation) :damage 114 :damage-type 'fire :initiative 7)
        (make-group :id 7 :type 'good :units 4033 :hit 8807 :weak '(radiation) :damage 21 :damage-type 'cold :initiative 5)
        (make-group :id 8 :type 'good :units 1209 :hit 7468 :weak '(fire) :immune '(cold) :damage 50 :damage-type 'radiation :initiative 20)
        (make-group :id 9 :type 'good :units 3228 :hit 7550 :weak '(cold) :immune '(bludgeoning radiation) :damage 21 :damage-type 'slashing :initiative 14)

        (make-group :id 0 :type 'bad :units 1230 :hit 36915 :weak '(cold slashing) :damage 58 :damage-type 'bludgeoning :initiative 16)
        (make-group :id 1 :type 'bad :units 629 :hit 23164 :damage 72 :damage-type 'slashing :initiative 11)
        (make-group :id 2 :type 'bad :units 266 :hit 16518 :damage 113 :damage-type 'fire :initiative 2)
        (make-group :id 3 :type 'bad :units 45 :hit 17769 :immune '(radiation slashing bludgeoning fire) :damage 774 :damage-type 'fire :initiative 19)
        (make-group :id 4 :type 'bad :units 93 :hit 32105 :weak '(fire) :damage 535 :damage-type 'fire :initiative 8)
        (make-group :id 5 :type 'bad :units 957 :hit 19599 :immune '(cold) :damage 32 :damage-type 'cold :initiative 15)
        (make-group :id 6 :type 'bad :units 347 :hit 29661 :damage 170 :damage-type 'bludgeoning :initiative 17)
        (make-group :id 7 :type 'bad :units 418 :hit 17587 :immune '(fire slashing) :weak '(radiation cold) :damage 73 :damage-type 'bludgeoning :initiative 6)
        (make-group :id 8 :type 'bad :units 2656 :hit 49851 :damage 32 :damage-type 'radiation :initiative 9)
        (make-group :id 9 :type 'bad :units 5365 :hit 35984 :damage 13 :damage-type 'radiation :initiative 13)))
