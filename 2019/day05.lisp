(in-package :advent-of-code-2019-day5)

(defun parse-input (input)
  (mapcar #'parse-integer (str:split "," input)))

(defparameter *code* nil)
(defparameter *input* nil)
(defparameter *output* nil)

(defmacro @ (index) `(aref *code* ,index))
;; (defmacro @@ (index) `(@ (aref *code* ,index)))

(defvar +opcodes+ (make-hash-table))

(eval-when (:compile-toplevel)
  (defun opcode (opcode args)
    (+ opcode
        (loop :for symb :in args
              :for multiplier := 100 :then (* multiplier 10)
              :sum (if (or (str:starts-with-p "@" (symbol-name symb))
                           (eq 'out symb))
                       0
                       multiplier))))

  (defun is-address (symb)
    (eql #\@ (char (symbol-name symb) 0)))

  (defun drop-first-letter (symb)
    (intern (subseq (symbol-name symb) 1)))

  (defun argument-name (symb)
    (if (is-address symb)
        (drop-first-letter symb)
        symb))

  (defun concat (lists)
    (apply #'concatenate 'list lists))

  (defun argument-permutations (args)
    (if args
        (let ((head (car args)))
          (if (eq 'out head)
              (mapcar (curry #'cons head) (argument-permutations (cdr args)))
              (concat (list (mapcar (curry #'cons head)
                                    (argument-permutations (cdr args)))
                            (mapcar (curry #'cons (intern (str:concat "@" (symbol-name head))))
                                    (argument-permutations (cdr args)))))))
        (list nil)))

  (defun find-argument (symb args)
    (find-if (curry #'eq symb) args :key #'argument-name)))

(defmacro defop (instruction argspec &body expr)
  (declare (type integer instruction))
  `(progn
     ,@(loop
         :for args :in (argument-permutations argspec)
         :collect (let* ((opcode (opcode instruction args))
                         (opname (intern (format nil "OP~D~{~A~}" opcode args))))
                    (labels ((wrap-indeces (symb)
                               (if (eq 'symbol (type-of symb))
                                   (let ((arg (find-argument symb args)))
                                     (if (is-address arg) `(@ ,symb) symb))
                                   symb)))
                      `(progn
                         (defun ,opname ,(loop :for arg :in args :collect (argument-name arg))
                           ,(if (member 'out args)
                                `(setf (@ out) (progn ,@(rutils:maptree #'wrap-indeces expr)))
                                `(progn ,@(rutils:maptree #'wrap-indeces expr))))
                         (setf (gethash ,opcode +opcodes+) (list #',opname ,(length args)))))))))

(progn
  (defop 1 (a b out) (+ a b))

  (defop 2 (a b out) (* a b))

  (defop 3 (out)
    (if (consp *input*)
        (pop *input*)
        (progn
          (format t "> ")
          (read))))

  (defop 4 (a) (push a *output*))

  (defop 5 (a b)
    (when (/= 0 a) (list 'ip b)))

  (defop 6 (a b)
    (when (zerop a) (list 'ip b)))

  (defop 7 (a b out)
    (if (< a b) 1 0))

  (defop 8 (a b out)
    (if (= a b) 1 0)))

(define-condition invalid-opcode (error)
  ((opcode :initarg :opcode :reader opcode)
   (pos :initarg :pos :reader pos)
   (code :initarg :code :reader code)))

(defun execute (code &optional input)
  (let ((*output* nil)
        (*input* input)
        (*code* (aocu:array-of-list (parse-input code))))
    (loop :with ip := 0 :do
      (let ((opcode (@ ip)))
        (if (= 99 opcode)
            (return (list *output* *code*))
            (if-it (gethash opcode +opcodes+)
                   (destructuring-bind (op argcount) it
                     (let ((eff (apply op (mapcar (curry #'aref *code*) (rutils:range (1+ ip) (+ ip argcount 1))))))
                       (if (and (consp eff) (eq 'ip (car eff)))
                           (setf ip (cadr eff))
                           (incf ip (1+ argcount)))))
                   (error 'invalid-opcode :code *code* :pos ip :opcode opcode)))))))

(deftest execute ()
    ;; simple case
  (should be equalp '(nil #(1002 4 3 4 99)) (execute "1002,4,3,4,33"))
  ;; comparisons
  (should be equalp '((1) #(3 9 8 9 10 9 4 9 99 1 8)) (execute "3,9,8,9,10,9,4,9,99,-1,8" '(8)))
  (should be equalp '((0) #(3 9 8 9 10 9 4 9 99 0 8)) (execute "3,9,8,9,10,9,4,9,99,-1,8" '(6)))
  (should be equalp '((0) #(3 9 7 9 10 9 4 9 99 0 8)) (execute "3,9,7,9,10,9,4,9,99,-1,8" '(9)))
  (should be equalp '((1) #(3 9 7 9 10 9 4 9 99 1 8)) (execute "3,9,7,9,10,9,4,9,99,-1,8" '(7)))
  (should be equalp '((0) #(3 3 1108 0 8 3 4 3 99)) (execute "3,3,1108,-1,8,3,4,3,99" '(9)))
  (should be equalp '((1) #(3 3 1108 1 8 3 4 3 99)) (execute "3,3,1108,-1,8,3,4,3,99" '(8)))
  (should be equalp '((0) #(3 3 1107 0 8 3 4 3 99)) (execute "3,3,1107,-1,8,3,4,3,99" '(8)))
  (should be equalp '((1) #(3 3 1107 1 8 3 4 3 99)) (execute "3,3,1107,-1,8,3,4,3,99" '(6)))
  ;; jumps
  (should be equalp '((0) #(3 12 6 12 15 1 13 14 13 4 13 99 0 0 1 9)) (execute "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" '(0)))
  (should be equalp '((1) #(3 12 6 12 15 1 13 14 13 4 13 99 10 1 1 9)) (execute "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" '(10)))
  (should be equalp '((0) #(3 3 1105 0 9 1101 0 0 12 4 12 99 0)) (execute "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" '(0)))
  (should be equalp '((1) #(3 3 1105 3 9 1101 0 0 12 4 12 99 1)) (execute "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" '(3)))
  ;; part 1
  (should be equalp
          '(9654885 0 0 0 0 0 0 0 0 0)
          (car (execute (aocu:get-input-for-day 5) '(1))))
  ;; part 2
  (should be equalp
          '(7079459)
          (car (execute (aocu:get-input-for-day 5) '(5)))))

(test :test 'execute)
