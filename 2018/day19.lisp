(cl:defpackage #:aoc201819
  (:use #:cl #:rutils #:optima))

(cl:in-package #:aoc201819)

(defparameter *registers* #(0 0 0 0 0 0))

(defmacro @ (register)
  `(aref *registers* ,register))

(defmacro defop (name op)
  (alexandria:with-gensyms (opr opi)
    (setf opr (alexandria:symbolicate name 'r))
    (setf opi (alexandria:symbolicate name 'i))
    `(progn
       (defun ,opr (A B C)
         (setf (@ C) (,op (@ A) (@ B))))
       (defun ,opi (A B C)
         (setf (@ C) (,op (@ A) B))))))

(defop add +)
(defop mul *)
(defop ban logand)
(defop bor logior)

(defun setr (A B C)
  (declare (ignore B))
  (setf (@ C) (@ A)))

(defun seti (A B C)
  (declare (ignore B))
  (setf (@ C) A))

(defun gtir (A B C)
  (if (> A (@ B))
      (setf (@ C) 1)
      (setf (@ C) 0)))

(defun gtri (A B C)
  (if (> (@ A) B)
      (setf (@ C) 1)
      (setf (@ C) 0)))

(defun gtrr (A B C)
  (if (> (@ A) (@ B))
      (setf (@ C) 1)
      (setf (@ C) 0)))

(defun eqir (A B C)
  (if (= A (@ B))
      (setf (@ C) 1)
      (setf (@ C) 0)))

(defun eqri (A B C)
  (if (= (@ A) B)
      (setf (@ C) 1)
      (setf (@ C) 0)))

(defun eqrr (A B C)
  (if (= (@ A) (@ B))
      (setf (@ C) 1)
      (setf (@ C) 0)))

(defparameter +IP+ 3)

(defparameter *program*
  #((addi 3 16 3)
    (seti 1 0 4)
    (seti 1 7 2)
    (mulr 4 2 1)
    (eqrr 1 5 1)
    (addr 1 3 3)
    (addi 3 1 3)
    (addr 4 0 0)
    (addi 2 1 2)
    (gtrr 2 5 1)
    (addr 3 1 3)
    (seti 2 6 3)
    (addi 4 1 4)
    (gtrr 4 5 1)
    (addr 1 3 3)
    (seti 1 3 3)
    (mulr 3 3 3)
    (addi 5 2 5)
    (mulr 5 5 5)
    (mulr 3 5 5)
    (muli 5 11 5)
    (addi 1 6 1)
    (mulr 1 3 1)
    (addi 1 13 1)
    (addr 5 1 5)
    (addr 3 0 3)
    (seti 0 6 3)
    (setr 3 1 1)
    (mulr 1 3 1)
    (addr 3 1 1)
    (mulr 3 1 1)
    (muli 1 14 1)
    (mulr 1 3 1)
    (addr 5 1 5)
    (seti 0 0 0)
    (seti 0 3 3)))

(defun fn->s (fn)
  (ecase fn
    (addi '+)
    (addr '+)
    (eqrr '=)
    (gtrr '>)
    (muli '*)
    (mulr '*)))

(defun r->s (index)
  (elt '(A B C IP E F) index))

(defun readable-call (fn a b)
  (ecase fn
    (seti a)
    (setr (r->s a))
    (gtrr (list (r->s a) '> (r->s b)))
    (eqrr (list (r->s a) '== (r->s b)))
    (muli (list (r->s a) '* b))
    (mulr (list (r->s a) '* (r->s b)))
    (addi (list (r->s a) '+ b))
    (addr (list (r->s a) '+ (r->s b)))))

(loop :for (label . sexp) :in (loop
                                :for instr :in (coerce *program* 'list)
                                :for i :from 0
                                :collect
                                (destructuring-bind (fn a b c) instr
                                  (cond
                                    ((and (eq fn 'seti) (= c 3))
                                     (list i 'jump (1+ a)))
                                    ((and (eq fn 'setr) (= c 3))
                                     (list i 'jump (list (r->s a) '+ 1)))
                                    ((= c 3)
                                     (list i 'jump (list (readable-call fn a b) '+ 1)))
                                    (t
                                     (list i
                                           (r->s c) '<-
                                           (readable-call fn a b))))))
      :do (format t "~3d ~a~%" label sexp))

(defparameter *registers* #(1 0 0 0 0 0))
(run 1)

  0 (JUMP ((IP + 16) + 1))
  1 (E <- 1)
  2 (C <- 1)

  3 (B <- (E * C))
  4 (B <- (B == F))
  5 (JUMP ((B + IP) + 1))
  6 (JUMP ((IP + 1) + 1))
  7 (A <- (E + A))
  8 (C <- (C + 1))
  9 (B <- (C > F))
 10 (JUMP ((IP + B) + 1)) ;; if (C > F) goto 12
 11 (JUMP 3)

 12 (E <- (E + 1))
 13 (B <- (E > F))
 14 (JUMP ((B + IP) + 1))
 15 (JUMP 2)
 16 (JUMP ((IP * IP) + 1))
 17 (F <- (F + 2))
 18 (F <- (F * F))
 19 (F <- (IP * F))
 20 (F <- (F * 11))
 21 (B <- (B + 6))
 22 (B <- (B * IP))
 23 (B <- (B + 13))
 24 (F <- (F + B))
 25 (JUMP ((IP + A) + 1))
 26 (JUMP 1)
 27 (B <- IP)
 28 (B <- (B * IP))
 29 (B <- (IP + B))
 30 (B <- (IP * B))
 31 (B <- (B * 14))
 32 (B <- (B * IP))
 33 (F <- (F + B))
 34 (A <- 0)
 35 (JUMP 1)
NIL

(defparameter *program*
  #((addi 3 16 3)
    (seti 1 0 4)
    (seti 1 7 2)
    (mulr 4 2 1)
    (eqrr 1 5 1)
    (addr 1 3 3)
    (addi 3 1 3)
    (addr 4 0 0)
    (addi 2 1 2)
    (gtrr 2 5 1)
    (addr 3 1 3)
    (seti 2 6 3)
    (addi 4 1 4)
    (gtrr 4 5 1)
    (addr 1 3 3)
    (seti 1 3 3)
    (mulr 3 3 3)
    (addi 5 2 5)
    (mulr 5 5 5)
    (mulr 3 5 5)
    (muli 5 11 5)
    (addi 1 6 1)
    (mulr 1 3 1)
    (addi 1 13 1)
    (addr 5 1 5)
    (addr 3 0 3)
    (seti 0 6 3)
    (setr 3 1 1)
    (mulr 1 3 1)
    (addr 3 1 1)
    (mulr 3 1 1)
    (muli 1 14 1)
    (mulr 1 3 1)
    (addr 5 1 5)
    (seti 0 0 0)
    (seti 0 3 3)))




(defparameter +IP+ 0)
(defparameter *program*
  #((seti 5 0 1)
    (seti 6 0 2)
    (addi 0 1 0)
    (addr 1 2 3)
    (setr 1 0 0)
    (seti 8 0 4)
    (seti 9 0 5)))



(defun ip ()
  (aref *registers* +IP+))

(defun instruction ()
  (let ((index (ip)))
    (when (and (>= index 0) (< index (length *program*)))
      (aref *program* index))))



(loop
  :with seen := (make-hash-table)
  :for instr := (instruction) :then (instruction)
  :while instr
  :do
     (when (gethash (ip) seen)
       (format t "ip = ~a registers = ~a~%" (ip) *registers*))
     (when instr
       (destructuring-bind (fn a b c) instr
         (funcall fn a b c))
       (setf (gethash (ip) seen) t)
       (incf (aref *registers* +IP+))))

(defun run (&optional (times))
  (loop
    :for i :from 0 :to (1- times)
    :do
       (let ((instr (instruction)))
         (format t "~a ~a~%" instr *registers*)
         (when instr
           (destructuring-bind (fn a b c) instr
             (funcall fn a b c))
           (incf (aref *registers* +IP+))))
    :finally
       (format t "ip = ~3d registers = ~a~%" (ip) *registers*)))

;; (setf *registers* #(0 10550400 0 1 0 10551381) => cycle starts

(setf *registers* #(1 0 10551382 9 2 10551381))

(setf *registers* #(1 0 3517127 3 3 10551381))

(loop :for i :from 2 :below (sqrt 10551381)
      :when (zerop (rem 10551381 i))
        :collect (list i (truncate 10551381 i)))

(+ 1 3 3517127 71 148611 213 49537 10551381)

14266944

(+ 1 )

(* 3 3517127)

(3 71 213)
