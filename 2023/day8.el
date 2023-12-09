(require 'cl)
(require 'subr-x)



;; day 8 part 1

(with-current-buffer (find-file-noselect "~/tmp/day8.txt")
  (let* ((tokens (string-split (buffer-string) "[\n ]+" t "[\s=,)(]*"))
         (instructions (car tokens))
         (network (seq-reduce (pcase-lambda (ht `(,target ,left ,right))
                                (setf (gethash target ht) (list ?L left ?R right))
                                ht)
                              (seq-partition (cdr tokens) 3)
                              (make-hash-table :test 'equal))))
    (let ((pos "AAA")
          (len (length instructions)))
      (loop for i from 0 ;; to 10
            until (string= pos "ZZZ")
            ;; do (message "%d %s %c" i pos (elt instructions (mod i len)))
            do (setf pos (getf (gethash pos network)
                               (elt instructions (mod i len))))
            finally (return i))))) ;; 12361



;; day 8 part 2

(with-current-buffer (find-file-noselect "~/tmp/day8.txt")
  (let* ((tokens (string-split (buffer-string) "[\n ]+" t "[\s=,)(]*"))
         (instructions (car tokens))
         (network (seq-reduce (pcase-lambda (ht `(,target ,left ,right))
                                (setf (gethash target ht) (list ?L left ?R right))
                                ht)
                              (seq-partition (cdr tokens) 3)
                              (make-hash-table :test 'equal))))
    (let ((pos (seq-filter (lambda (key) (eq ?A (elt key 2))) (hash-table-keys network)))
          (len (length instructions)))
      (seq-reduce (lambda (acc p)
                    (calcFunc-lcm acc
                                  (loop for i from 0 to 100000
                                        until (eq ?Z (elt p 2))
                                        ;; do (message "%d %s %c" i p (elt instructions (mod i len)))
                                        do (setf p (getf (gethash p network) (elt instructions (mod i len))))
                                        finally (return i))))
                  pos 1)))) ;; 18215611419223

