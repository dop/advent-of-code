(require 'cl)
(require 'dash)



;; day 7 part 1

(defun card-rank (card)
  (seq-position "23456789TJQKA" card))

(defun counthash (key table)
  (setf (gethash key table) (1+ (gethash key table 0))))

(defun hand-stats (hand)
  (seq-reduce (lambda (acc c) (counthash c acc) acc) hand (make-hash-table)))

;; Five of a kind, where all five cards have the same label: AAAAA
(defun five-of-kind-p (stats)
  (= 1 (hash-table-count stats)))

(five-of-kind-p "AAAAA")
(five-of-kind-p "AAAA2")

;; Four of a kind, where four cards have the same label and one card has a different label: AA8AA
(defun four-of-kind-p (stats)
  (and (= 2 (hash-table-count stats))
       (memq 1 (hash-table-values stats))))

(four-of-kind-p "AAABA")
(four-of-kind-p "AAABB")

;; Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
(defun full-house-p (stats)
  (and (= 2 (hash-table-count stats))
       (memq 3 (hash-table-values stats))))

(full-house-p "AAABA")
(full-house-p "AAABB")

;; Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
(defun three-of-kind-p (stats)
  (and (= 3 (hash-table-count stats))
       (memq 3 (hash-table-values stats))))

;; Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
(defun two-pair-p (stats)
  (and (= 3 (hash-table-count stats))
       (memq 2 (hash-table-values stats))))

;; One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
(defun one-pair-p (stats)
  (and (= 4 (hash-table-count stats))
       (memq 2 (hash-table-values stats))))

;; High card, where all cards' labels are distinct: 23456
(defun high-card-p (stats)
  (= 5 (hash-table-count stats)))

(defun hand-rank (hand)
  (seq-position '(high-card-p one-pair-p two-pair-p three-of-kind-p full-house-p four-of-kind-p five-of-kind-p)
                (hand-stats hand)
                (lambda (fn stats) (funcall fn stats))))

(defun hand< (hand1 hand2)
  (loop for i from 0 to 4 do
        (let ((r1 (card-rank (elt hand1 i)))
              (r2 (card-rank (elt hand2 i))))
          (cond ((< r1 r2) (return t))
                ((> r1 r2) (return nil))))))

;; So, the first step is to put the hands in order of strength:

;; 32T3K is the only one pair and the other hands are all a stronger
;; type, so it gets rank 1.

;; KK677 and KTJJT are both two pair. Their first cards both have the
;; same label, but the second card of KK677 is stronger (K vs T), so
;; KTJJT gets rank 2 and KK677 gets rank 3.

;; T55J5 and QQQJA are both three of a kind. QQQJA has a stronger
;; first card, so it gets rank 5 and T55J5 gets rank 4.

;; Now, you can determine the total winnings of this set of hands by
;; adding up the result of multiplying each hand's bid with its rank
;; (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5). So the total
;; winnings in this example are 6440.

(with-current-buffer (find-file-noselect "~/tmp/day7.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " +"))))
    (->> (string-lines (buffer-string))

         (mapcar (lambda (line)
                   (pcase-exhaustive (string-split line " +")
                     (`(,hand ,bid)
                      (cons hand (number bid))))))

         (seq-sort (pcase-lambda (`(,hand1 . _) `(,hand2 . _))
                     (let ((r1 (hand-rank hand1))
                           (r2 (hand-rank hand2)))
                       (if (= r1 r2)
                           (hand< hand1 hand2)
                         (< r1 r2)))))

         (seq-map-indexed (pcase-lambda (`(_ . ,bid) index)
                            (* bid (1+ index))))

         (apply #'+)))) ;; 246912307



;; day 7 part 2

(defun card-rank-2 (card)
  (seq-position "J23456789TQKA" card))

(defun hand2< (hand1 hand2)
  (loop for i from 0 to 4 do
        (let ((r1 (card-rank-2 (elt hand1 i)))
              (r2 (card-rank-2 (elt hand2 i))))
          (cond ((< r1 r2) (return t))
                ((> r1 r2) (return nil))))))

(defun upgrade-jokers (stats)
  (when-let ((jokers (gethash ?J stats))
             (best-cards
              (sort (seq-remove (lambda (a) (eq ?J (car a)))
                                (hash-table-alist stats))
                    (lambda (a b) (> (cdr a) (cdr b))))))
    (incf (gethash (caar best-cards) stats) jokers)
    (remhash ?J stats))
  stats)

(defun hand-rank2 (hand)
  (seq-position '(high-card-p one-pair-p two-pair-p three-of-kind-p full-house-p four-of-kind-p five-of-kind-p)
                (upgrade-jokers (hand-stats hand))
                (lambda (fn stats) (funcall fn stats))))

;; (hash-table-alist (upgrade-jokers (hand-stats "2345J")))

;; (hash-table-alist (upgrade-jokers (hand-stats "AA4JJ")))

;; (hash-table-alist (hand-stats "2345J")) ((74 . 1) (53 . 1) (52 . 1) (51 . 1) (50 . 1))

;; (hash-table-alist (hand-stats "AA4JJ")) ((74 . 2) (52 . 1) (65 . 2))


(with-current-buffer (find-file-noselect "~/tmp/day7.txt")
  (labels ((number (str) (string-to-number str))
           (numbers (str) (mapcar #'number (string-split str " +"))))
    (->> (string-lines (buffer-string))

         (mapcar (lambda (line)
                   (pcase-exhaustive (string-split line " +")
                     (`(,hand ,bid)
                      (cons hand (number bid))))))

         (seq-sort (pcase-lambda (`(,hand1 . _) `(,hand2 . _))
                     (let ((r1 (hand-rank2 hand1))
                           (r2 (hand-rank2 hand2)))
                       (if (= r1 r2)
                           (hand2< hand1 hand2)
                         (< r1 r2)))))

         (seq-map-indexed (pcase-lambda (`(_ . ,bid) index)
                            (* bid (1+ index))))

         (apply #'+)))) ;; 246894760

