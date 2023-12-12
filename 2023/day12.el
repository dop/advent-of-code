;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)

(defmacro with-output-buffer (name &rest body)
  (declare (indent 1))
  `(let ((standard-output (if (stringp ,name)
                              (get-buffer-create ,name)
                            ,name)))
     (when (bufferp standard-output)
       (with-current-buffer standard-output
         (erase-buffer)))
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



;; day 12 part 1

(with-puzzle (:in "~/tmp/day12.example.txt" :out t)
  (loop for line in (string-lines (buffer-string))
        sum (let* ((pattern-regions (string-split line "[ ,]" t))
                   (pattern (car pattern-regions))
                   (regions (mapcar #'string-to-number (cdr pattern-regions)))
                   (match   (mapcar (lambda (count) (seq-into (loop repeat count collect ?#) 'string))
                                    regions)))
              (seq-length
               (seq-filter
                (lambda (pattern)
                  (equal match (string-split pattern "[^#]" t)))
                (loop with patterns = (list pattern)
                      for i from 0 below (length pattern)
                      when (eq ?? (elt pattern i))
                      do (progn
                           (mapc (lambda (pattern) (setf (elt pattern i) ?.)) patterns)
                           (setf patterns
                                 (append patterns
                                         (mapcar (lambda (pattern)
                                                   (let ((new-pattern (seq-copy pattern)))
                                                     (setf (elt new-pattern i) ?#)
                                                     new-pattern))
                                                 (seq-copy patterns)))))
                      finally (return patterns))))
              ))) ;; 7221



;; day 12 part 2

(with-puzzle (:in "~/tmp/day12.txt")

  (let ((dp (make-hash-table :test 'equal)))

    (defun counts (pattern regions pi ri run)
      (with-cache (list pi ri run) dp
        (cond ((< pi (length pattern))
               (let ((el (elt pattern pi))
                     (rg (elt regions ri)))
                 (cond ((plusp run)
                        (cond ((or (not rg) (> run rg))
                               0)
                              ((eq el ??)
                               ;; (print (list :guess-cont-region pattern regions pi ri run))
                               (+ (if (< run rg)
                                      (counts pattern regions (1+ pi) ri (1+ run))
                                    0)
                                  (if (= run rg)
                                      (counts pattern regions (1+ pi) (1+ ri) 0)
                                    0)))
                              ((eq el ?.)
                               (if (= rg run)
                                   (progn
                                     ;; (print (list :done-region pattern regions pi ri run))
                                     (counts pattern regions (1+ pi) (1+ ri) 0))
                                 0))
                              ((eq el ?#)
                               ;; (print (list :grow-region pattern regions pi ri run))
                               (counts pattern regions (1+ pi) ri (1+ run)))))
                       (t
                        (cond ((eq el ??)
                               ;; (print (list :guess-start-region pattern regions pi ri run))
                               (+ (counts pattern regions (1+ pi) ri 1)
                                  (counts pattern regions (1+ pi) ri 0)))
                              ((eq el ?.)
                               ;; (print (list :no-region pattern regions pi ri run))
                               (counts pattern regions (1+ pi) ri 0))
                              ((eq el ?#)
                               ;; (print (list :start-region pattern regions pi ri run))
                               (counts pattern regions (1+ pi) ri 1)))))))
              ((and (= pi (length pattern))
                    (or (and (= ri (length regions))
                             (= run 0))
                        (and (= ri (1- (length regions)))
                             (= run (elt regions ri)))))
               ;; (print (list :ok pattern regions pi ri run))
               1)
              (t
               ;; (print (list :deadend pattern regions pi ri run))
               0))))

    (loop with lines = (string-lines (buffer-string))
          ;; with len = (length lines)
          ;; for i from 1
          for line in lines
          sum (let* ((pattern-regions (string-split line "[ ,]" t))
                     (pattern (car pattern-regions))
                     (regions (mapcar #'string-to-number (cdr pattern-regions))))
                (clrhash dp)
                ;; (princ (format "%d/%d\n" i len))
                (counts (string-join (loop repeat 5 collect pattern) "?")
                        (loop repeat 5 append regions)
                        0 0 0))))) ;; 7139671893722
