(defstruct player
  (life 0 :type number)
  (damage 0 :type number)
  (armor 0 :type number))

(defparameter *spells*
  '((magic-missile . 53)
    (drain . 73)
    (shield . 113)
    (poison . 173)
    (recharge . 229)))

(defparameter *smallest* 10000)
(defparameter *spent-mana* 0)
(defparameter *mana* 0)
(defparameter *wizard* (make-player))
(defparameter *boss* (make-player))
(defparameter *active-spells* '())

(defun activate-spell (type amount timer)
  (push (list type amount timer) *active-spells*))

(defun active-spells ()
  *active-spells*)

(defun cast-spell (name amount)
  (case name
    (damage
       (decf (player-life *boss*) amount))
    (mana
       (incf *mana* amount))))

(defun apply-active-spells ()
  (setf *active-spells*
        (loop for (spell amount timer) in (active-spells)
              do
                 ;; (format t "Active spell ~A ~A.~%" spell amount)
                 (cast-spell spell amount)
              when (> timer 1)
                collect (list spell amount (1- timer)))))

(defun wizard-armor ()
  (rutils:if-it (find 'armor (active-spells) :key #'car)
                (second rutils:it)
                0))

(defun spend-mana (amount)
  (decf *mana* amount)
  (incf *spent-mana* amount))

(defun wizard-turn (spell cost)
  ;; (format t "Wizard casts ~A (cost ~A).~%" spell cost)
  (spend-mana cost)
  (case spell
    (magic-missle
     (decf (player-life *boss*) 4))
    (drain
     (decf (player-life *boss*) 2)
     (incf (player-life *wizard*) 2))
    (shield
     (activate-spell 'armor 7 6))
    (poison
     (activate-spell 'damage 3 6))
    (recharge
     (activate-spell 'mana 101 5)))
  (play 'boss))

(defun applicable-spells ()
  (remove-if (lambda (spell)
               (destructuring-bind (name . cost) spell
                 (or (member name (active-spells))
                     (> cost *mana*))))
             *spells*))

(defun play (turn)
  ;; (format t "~A turn, boss life ~A, wizard life ~A, mana ~A, spent ~A.~%" turn (player-life *boss*) (player-life *wizard*) *mana* *spent-mana*)
  (apply-active-spells)
  (unless (plusp (player-life *boss*))
    (format t "Boss defeated! Total mana spent ~A.~%" *spent-mana*)
    (setf *smallest* (min *smallest* *spent-mana*))
    (return-from play *spent-mana*))
  (unless (plusp (player-life *wizard*))
    (return-from play *smallest*))
  (case turn
    (wizard
       (loop for (spell . cost) in (applicable-spells)
             minimize (let ((*mana* *mana*)
                            (*spent-mana* *spent-mana*)
                            (*boss* (make-player :life 13 :damage 8))
                            (*wizard* (make-player :life 10))
                            (*active-spells* (mapcar #'identity *active-spells*)))
                        (wizard-turn spell cost))))
    (boss
       (let ((damage (max 1 (- (player-damage *boss*) (wizard-armor)))))
         ;; (format t "Boss deals ~A damage.~%" damage)
         (decf (player-life *wizard*) damage)
         (play 'wizard)))))

(let ((*mana* 250)
      (*spent-mana* 0)
      (*boss* (make-player :life 13 :damage 8))
      (*wizard* (make-player :life 10))
      (*active-spells* '()))
  (play 'wizard))
