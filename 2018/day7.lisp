(ql:quickload '(:rutils :cl-ppcre))
(cl:defpackage #:aoc-2018-day7 (:use #:cl #:rutils #:cl-ppcre))
(cl:in-package #:aoc-2018-day7)

(defconstant *input*
  '((Z V)
    (V K)
    (M Q)
    (E X)
    (J W)
    (L O)
    (Q T)
    (Y P)
    (X R)
    (T U)
    (I O)
    (P H)
    (G A)
    (N A)
    (H B)
    (F D)
    (S O)
    (O W)
    (D U)
    (W B)
    (A K)
    (B R)
    (K C)
    (R C)
    (U C)
    (A U)
    (J I)
    (D K)
    (V S)
    (H C)
    (R U)
    (I G)
    (D R)
    (M B)
    (G R)
    (M I)
    (G N)
    (M N)
    (Q S)
    (I S)
    (J R)
    (O B)
    (G S)
    (J C)
    (M D)
    (T H)
    (P N)
    (S K)
    (T C)
    (J A)
    (G F)
    (N R)
    (N W)
    (T I)
    (S B)
    (H F)
    (B C)
    (L W)
    (N O)
    (O A)
    (H S)
    (F A)
    (F C)
    (M A)
    (Z H)
    (Z L)
    (E H)
    (X T)
    (Y X)
    (E W)
    (P R)
    (Z E)
    (W C)
    (I P)
    (X A)
    (Y C)
    (I F)
    (L T)
    (A B)
    (F W)
    (T R)
    (X F)
    (M O)
    (N K)
    (T S)
    (J N)
    (J S)
    (O D)
    (T P)
    (Z D)
    (L X)
    (Q G)
    (M G)
    (P W)
    (V P)
    (D B)
    (Y D)
    (X S)
    (K U)
    (Z Y)
    (D W)))

(defconstant *example*
  '((C A)
    (C F)
    (A B)
    (A D)
    (B E)
    (D E)
    (F E)))

(defun next-goals (dependency-graph)
  (loop
    with firsts = ()
    for dependencies being the hash-values in dependency-graph using (hash-key goal)
    when (null dependencies)
      do (push goal firsts)
    finally
       (return (sort firsts (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))

(defun next-goal (dependency-graph)
  (car (next-goals dependency-graph)))

(let ((dependency-graph (make-hash-table :test 'eq)))

  (loop for (dependency goal) in *input* do
    (if (null (gethash dependency dependency-graph))
        (setf (gethash dependency dependency-graph) nil))
    (push dependency (gethash goal dependency-graph '())))

  (loop
    with task-list = ()
    for next-task = (next-goal dependency-graph) then (next-goal dependency-graph)
    while next-task
    do
       ;; (format t "next task ~a~%" next-task)
       (push next-task task-list)
       (remhash next-task dependency-graph)
       (loop
         for dependencies being the hash-values in dependency-graph using (hash-key goal)
         do
            (setf (gethash goal dependency-graph)
                  (remove next-task dependencies)))
       ;; (format t "dependency graph ~a~%" (hash-table-to-alist dependency-graph))
    finally
       (return (nreverse task-list)))) ;; => JMQZELVYXTIGPHFNSOADKWBRUC

(let ((dependency-graph (make-hash-table :test 'eq))
      (workers ())
      (extra-cost 60)
      (max-workers 5)
      (time 0))

  (loop for (dependency goal) in *input* do
    (if (null (gethash dependency dependency-graph))
        (setf (gethash dependency dependency-graph) nil))
    (push dependency (gethash goal dependency-graph)))

  (defun task-cost (x)
    (+ extra-cost (- (char-code (char (symbol-name x) 0)) 64)))

  (defun finish-goal (goal)
    (remhash goal dependency-graph)
    (loop
      :for deps :being :the :hash-values :in dependency-graph :using (hash-key key)
      :do (setf (gethash key dependency-graph) (remove goal deps))))

  (defun enque-work (goals)
    (loop :for goal :in goals :do
      (unless (or (>= (length workers) max-workers) (member goal workers :key #'car))
        (push (cons goal (task-cost goal)) workers))))

  (defun advance-work ()
    (let* ((cost (apply #'min (mapcar #'cdr workers)))
           (advanced-workers (mapcar (lambda (worker) (cons (car worker) (- (cdr worker) cost))) workers))
           (finished-goals (remove-if-not (lambda (worker) (zerop (cdr worker))) advanced-workers)))
      (setf workers (remove-if (lambda (worker) (zerop (cdr worker))) advanced-workers))
      (loop :for (goal . cost) :in finished-goals :do (finish-goal goal))
      cost))

  (loop
    :for next-goals := (next-goals dependency-graph) :then (next-goals dependency-graph)
    :while (or next-goals (plusp (hash-table-count dependency-graph)))
    :do
       (enque-work next-goals)
       (incf time (advance-work)))

  time) ;; => 1133
