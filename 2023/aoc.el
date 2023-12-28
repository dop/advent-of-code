;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)

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
     ,@body))

(defmacro with-puzzle (options &rest body)
  (declare (indent 1))
  (let ((time (gensym "time"))
        (result (gensym "result")))
    `(with-output-buffer
         ,(let ((default (if noninteractive t "*debug*")))
            (cl-typecase options
              (string default)
              (t (getf options :out default))))
       (with-current-buffer
           (find-file-noselect ,(cl-typecase options
                                  (string options)
                                  (t (getf options :in))))
         (let ((,time (current-time))
               (,result (progn ,@body)))
           (princ (format "=> %s\nin %0.6f sec."
                          ,result
                          (float-time (time-since ,time)))))))))

(defmacro with-cache (key table &rest body)
  (declare (indent 2))
  `(or (gethash ,key ,table)
       (setf (gethash ,key ,table)
             (progn ,@body))))

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

(defun print-block (rows cols block printfn &optional newlinefn)
  (loop for r from 0 below rows
        do (loop for c from 0 below cols
                 do (let ((v (aref block (+ c (* r cols)))))
                      (funcall printfn r c v)))
        do (or (and newlinefn (funcall newlinefn))
               (princ "\n"))))

(defun print-grid (rows cols block &optional overlayfn)
  (print-block rows cols block
               (lambda (r c val)
                 (princ (or (and overlayfn (funcall overlayfn r c)) val)))))

(defstruct matrix
  (rows :type fixnum)
  (cols :type fixnum)
  (data :type vector))

(defun matrix (rows cols data)
  (make-matrix :rows rows :cols cols :data data))

(defun print-matrix (m)
  (with-slots (rows cols data) m
    (print-block rows cols data (lambda (r c v) (princ (format "% 7.1f" v))))))

(defun gaussian-elimination (matrix)
  (with-slots (rows cols data) matrix
    (assert (= (1+ rows) cols))
    (cl-macrolet ((ref (r c)
                    `(aref data (+ ,c (* ,r cols)))))
      ;; (pri rows cols)
      (loop for i from 0 below (1- rows)
            ;; do (pri i)
            do (assert (not (= 0 (ref i i))))
            do (loop for j from (1+ i) below rows
                     ;; do (pri j)
                     do (let ((r (/ (float (ref j i)) (ref i i))))
                          ;; (pri r '= (ref j i) '/ (ref i i))
                          (loop for k from 0 below cols
                                ;; do (pri i j k (ref j k) '- (* r (ref i k)) '= (- (ref j k) (* r (ref i k))))
                                do (decf (ref j k) (* r (ref i k)))))))

      ;; (print-matrix matrix)
      ;; back substitute
      (loop for i from (1- rows) downto 0
            ;; do (pri i (1- rows) (1+ i))
            do (loop for j from (1- rows) downto (1+ i)
                     ;; do (pri j)
                     do (decf (ref i (1- cols))
                              (* (ref i j) (ref j (1- cols))))
                     do (setf (ref i j) 0)
                     finally
                     ;; (pri :fin i j (ref i j))
                     (setf (ref i (1- cols)) (/ (ref i (1- cols))
                                                (ref i j))
                           (ref i j) 1))
            ;; do (print-matrix matrix)
            ))))
