(in-package :advent-of-code-2019-day2)

(defparameter *input*
  (mapcar #'parse-integer (str:split "," (aocu:get-input-for-day 2))))

(defparameter *code* (aocu:array-of-list *input*))

(defmacro @@ (index) `(aref *code* (aref *code* ,index)))

(defvar +opcodes+ (make-hash-table))

(defmacro defop (opcode args &body expr)
  (declare (type integer opcode))
  (let ((opname (intern (format nil "OP~D" opcode))))
    (flet ((wrap-indeces (symb)
             (if (member symb args)
                 `(@@ ,symb)
                 symb)))
      `(progn
         (defun ,opname ,args
           (setf (@@ out) ,@(rutils:maptree #'wrap-indeces expr)))
         (setf (gethash ,opcode +opcodes+) (list #',opname ,(length args)))))))

(defop 1 (a b out)
  (+ a b))

(defop 2 (a b out)
  (* a b))

(define-condition invalid-opcode (error)
  ((opcode :initarg :opcode :reader opcode)
   (pos :initarg :pos :reader pos)
   (code :initarg :code :reader code)))

(defun execute (&optional (*code* *code*))
  (loop :with i := 0 :do
    (let ((opcode (aref *code* i)))
      (if (= 99 opcode)
          (return *code*)
          (if-it (gethash opcode +opcodes+)
                 (destructuring-bind (op argcount) it
                   (apply op (rutils:range (1+ i) (+ i argcount 1)))
                   (incf i (1+ argcount)))
                 (error 'invalid-opcode :code *code* :pos i :opcode opcode))))))

;; Part 1
(progn
  (setf *code* (aocu:array-of-list *input*)
        (aref *code* 1) 12
        (aref *code* 2) 2)
  (aref (execute *code*) 0)) ; => 5098658 (23 bits, #x4DCCA2)

;; Part 2
(loop :named outer :for noun :from 0 :to 99 :do
  (loop :for verb :from 9 :to 99 :do
    (setf *code* (aocu:array-of-list *input*)
          (aref *code* 1) noun
          (aref *code* 2) verb)
    (execute)
    (when (= 19690720 (aref *code* 0))
      (return-from outer (+ verb (* 100 noun)))))) ; => 5064 (13 bits, #x13C8)
