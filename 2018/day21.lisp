(cl:defpackage #:aoc201821
  (:use #:cl #:rutils #:optima))

(cl:in-package #:aoc201821)

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

(defparameter +IP+ 2)

(defparameter *program*
  #((seti 123 0 1)
    (bani 1 456 1)
    (eqri 1 72 1)
    (addr 1 2 2)
    (seti 0 0 2)
    (seti 0 4 1)
    (bori 1 65536 3)
    (seti 10905776 4 1)
    (bani 3 255 4)
    (addr 1 4 1)
    (bani 1 16777215 1)
    (muli 1 65899 1)
    (bani 1 16777215 1)
    (gtir 256 3 4)
    (addr 4 2 2)
    (addi 2 1 2)
    (seti 27 1 2)
    (seti 0 6 4)
    (addi 4 1 5)
    (muli 5 256 5)
    (gtrr 5 3 5)
    (addr 5 2 2)
    (addi 2 1 2)
    (seti 25 1 2)
    (addi 4 1 4)
    (seti 17 9 2)
    (setr 4 7 3)
    (seti 7 4 2)
    (eqrr 1 0 4)
    (addr 4 2 2)
    (seti 5 1 2)))

(defun r->s (index)
  (elt '(A B IP D E F) index))

(defun readable-call (fn a b)
  (ecase fn
    (seti a)
    (setr (r->s a))
    (bani (list (r->s a) 'and b))
    (bori (list (r->s a) 'or b))
    (gtir (list a '> (r->s b)))
    (gtrr (list (r->s a) '> (r->s b)))
    (eqrr (list (r->s a) '== (r->s b)))
    (eqri (list (r->s a) '== b))
    (muli (list (r->s a) '* b))
    (mulr (list (r->s a) '* (r->s b)))
    (addi (list (r->s a) '+ b))
    (addr (list (r->s a) '+ (r->s b)))))

(defparameter *code*
  (coerce
   (loop :for (label . sexp) :in (loop
                                   :for instr :in (coerce *program* 'list)
                                   :for i :from 0
                                   :collect
                                   (destructuring-bind (fn a b c) instr
                                     (cond
                                       ((and (eq fn 'seti) (= c +IP+))
                                        (list i 'jump (1+ a)))
                                       ((and (eq fn 'setr) (= c +IP+))
                                        (list i 'jump (list (r->s a) '+ 1)))
                                       ((= c +IP+)
                                        (list i 'jump (list (readable-call fn a b) '+ 1)))
                                       (t
                                        (list i
                                              (r->s c) '<-
                                              (readable-call fn a b))))))
         :collect (format nil "~3d ~a" label sexp))
   'vector))

(defun ip ()
  (aref *registers* +IP+))

(defun instruction ()
  (let ((index (ip)))
    (when (and (>= index 0) (< index (length *program*)))
      (aref *program* index))))

(defun print-registers ()
  (format t "A: ~15a, B: ~15a, D: ~15a, E: ~15a, F: ~15a~%"
          (aref *registers* 0)
          (aref *registers* 1)
          ;; (aref *registers* 2)
          (aref *registers* 3)
          (aref *registers* 4)
          (aref *registers* 5)))

(defun run (&optional (times))
  (loop
    :with seen := (make-hash-table) :and last
    ;; :for i :from 0 :to (1- times)
      :initially
         (print-registers)
    :do
       (when-let (instr (instruction))
         (destructuring-bind (fn a b c) instr
           ;; (format t "~30a" (aref *code* (ip)))
           (funcall fn a b c)
           ;; (print-registers)
           (when (= (ip) 28)
             (let ((b (aref *registers* 1)))
               (when (gethash b seen)
                 (return last))
               (setf last b
                     (gethash b seen) t)
             (format t "~a~%" (aref *registers* 1))))
         (incf (aref *registers* +IP+))))))

(defun squares (x)
  (loop :for n := 1 :then (* n 2)
        :while (<= n x)
        :when (plusp (logand x n))
          :collect n))

(defun factors (n)
  (let ((factors nil))
    (loop :for i :from 2 :to (sqrt n) :do
      (when (zerop (rem n i))
        (push i factors)
        (let ((k (truncate n i)))
          (when (/= k i)
            (push k factors)))))
    (cons 1 (cons n factors))))

(defun hash (x)
  (logand (* (logand x 16777215) 65899) 16777215))

;; 11285115

(let ((seen (make-hash-table))
      (last nil)
      (times 0)
      (e 0)
      (b 0)
      (d 0))
  (loop :named main :do
    (setf d (logior b 65536)
          b 10905776)
    (loop :do
      (setf e (logand d 255)
            b (hash (+ b e)))
      (if (> 256 d)
          (if (gethash b seen)
              (return-from main last)
              (progn
                ;; (when (> (incf times) 5)
                ;;   (return-from main))
                ;; (format t "~a~%" b)
                (setf last b
                      (gethash b seen) t)
                (return)))
          (setf d (ash d -8))))))

;; 2947113
;; 9635632 too high

;; 11285115
;; 14649954
;; 14236869
;; 14914558
;; 7052678
;; 1820592
;; 519736
;; 9762083
;; 10862269
;; 3787164
;; 7661496
