;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)

(defalias 'partial #'apply-partially)

(defun print-grid (rows cols block)
  (loop for r from 0 below rows
        do (loop for c from 0 below cols
                 do (princ (format "%c" (aref block (+ c (* r cols))))))
        do (princ "\n")))

(defun pr (template &rest args)
  (princ (apply #'format template args))
  (terpri))

(defun neq (a b)
  (not (eq a b)))

(defun set-add (set value)
  (setf (gethash value set) t))

(defun counthash (key table)
  (setf (gethash key table) (1+ (gethash key table 0))))

(defmacro with-output-buffer (name &rest body)
  (declare (indent 1))
  `(let ((standard-output (if (stringp ,name)
                              (get-buffer-create ,name)
                            ,name)))
     (when (bufferp standard-output)
       (with-current-buffer standard-output
         (erase-buffer)
         (redisplay)))
     (print (progn ,@body))))

(defmacro with-puzzle (options &rest body)
  (declare (indent 1))
  `(with-output-buffer
       ,(cl-typecase options
          (string "*debug*")
          (t (getf options :out "*debug*")))
     (with-current-buffer
         (find-file-noselect ,(cl-typecase options
                                (string options)
                                (t (getf options :in))))
       ,@body)))

(defmacro with-cache (key table &rest body)
  (declare (indent 2))
  `(or (gethash ,key ,table)
       (setf (gethash ,key ,table)
             (progn ,@body))))

(defun seq-count-eq (el sequence)
  (seq-count (lambda (e) (eq el e)) sequence))

(defun palindrome-p (seq)
  (let ((len (length seq)))
    (loop for i from 0 below (/ len 2)
          always (eq (elt seq i) (elt seq (- len i 1))))))

(defun read-grid (raw)
  (let* ((grid (string-trim raw))
         (cols (seq-position grid ?\n))
         (rows (1+ (seq-count (partial #'eq ?\n) grid)))
         (block (string-replace "\n" "" grid)))
    (list rows cols block)))

(defun print-grid (rows cols block)
  (loop for r from 0 below rows
        do (loop for c from 0 below cols
                 do (princ (format "%c" (aref block (+ c (* r cols))))))
        do (princ "\n")))

(defmacro collecting (collectors &rest body)
  (declare (indent 1))
  (let ((single (atom collectors))
        (value  (gensym "value")))
    (when single
      (setq collectors (list collectors)))
    `(let ,collectors
       (cl-flet ,(mapcar (lambda (collector)
                           `(,collector (,value) (push ,value ,collector) ,value))
                         collectors)
         ,@body
         ,(if single
              `(nreverse ,(car collectors))
            `(mapcar #'nreverse (list ,@collectors)))))))

(defun hash-table-alist (table)
  "Construct an alist from a TABLE hash table."
  (let ((alist nil))
    (maphash (lambda (key value) (push (cons key value) alist)) table)
    alist))
