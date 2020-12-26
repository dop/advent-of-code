(defpackage #:day23-2015
  (:use #:cl)
  (:import-from #:dop #:fn #:kw)
  (:import-from #:trivia #:match #:ematch #:vector*)
  (:import-from #:rutils #:->> #:% #:xor #:it)
  (:import-from #:trivia.ppcre #:split #:ppcre))

(in-package #:day23-2015)

(defun parse-instruction (line)
  (ematch line
    ((ppcre "jmp ([+-][0-9]+)" (read offset))
     (list :jmp offset))
    ((ppcre "([a-z]{3}) (a|b)(, ([+-][0-9]+))?" instr reg _ arg)
     (cons (kw instr)
           (cons (kw reg)
                 (and arg (list (parse-integer arg))))))))

(defparameter +input+
  (->> (aoc:puzzle 23 2015)
       (str:lines)
       (mapcar #'parse-instruction)
       (coerce % 'vector)))

(let ((regs (list :a 1 :b 0))
      (ip 0))
  (macrolet ((reg (r) `(getf regs ,r)))
    (loop for i from 0 while (< ip (length +input+)) do
      (format t "~{~a ~5d ~} ~3d ~a~%" regs ip (elt +input+ ip))
      (ematch (elt +input+ ip)
        ((list :hlf r)
         (setf (reg r) (floor (reg r) 2)))
        ((list :tpl r)
         (setf (reg r) (* 3 (reg r))))
        ((list :inc r)
         (incf (reg r)))
        ((list :jmp o)
         (incf ip (1- o)))
        ((list :jie r o)
         (incf ip (if (evenp (reg r)) (1- o) 0)))
        ((list :jio r o)
         (incf ip (if (= (reg r) 1) (1- o) 0))))
      (incf ip)))
  regs)
