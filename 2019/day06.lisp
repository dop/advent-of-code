(in-package :advent-of-code-2019-day6)

(defparameter *example* "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(defun parse-input (input)
  (mapcar (curry #'str:split ")") (str:lines input)))

(defun orbitmap (input &optional reverse)
  (loop :with ht := (make-hash-table :test #'equalp)
        :for (a b) :in (parse-input input)
        :do (if reverse (push a (gethash b ht)) (push b (gethash a ht)))
        :finally (return ht)))

(defun part1 (input)
  (let ((hm (orbitmap input)))
    (loop :for orbitoree :being :the :hash-keys :of hm :using (:hash-value orbitors)
          :sum (loop
                 :with q := (copy-list orbitors)
                 :for hop := (pop q)
                 :while hop
                 :do
                    (loop :for o :in (gethash hop hm) :do (push o q))
                 :count t))))

(should be = (part1 *example*) 42)
(should be = (part1 (aocu:get-input-for-day 6)) 144909)

(defparameter *example2* "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(defconstant +infinity+ 1238712894719823718923)

(defun part2 (input)
  (let* ((current "YOU")
         (target "SAN")
         (visited (make-hash-table :test #'equalp))
         (satellites (orbitmap input))
         (planets (orbitmap input t)))
    (labels ((planets (of)
               (gethash of planets))
             (visited-p (object)
               (gethash object visited))
             (mark-as-visited (object)
               (setf (gethash object visited) t))
             (satellites (of)
               (gethash of satellites))
             (path-from (object)
               (loop
                 :with best := +infinity+
                 :for next :in (concatenate 'list (planets object) (satellites object)) :do
                   (unless (visited-p next)
                     (mark-as-visited next)
                     (if (equalp next target)
                         (return 0)
                         (setf best (min best (1+ (path-from next))))))
                 :finally
                    (return best))))
      (1- (path-from "YOU")))))

(should be = (part2 *example2*) 4)
(should be = (part2 (aocu:get-input-for-day 6)) 259)
