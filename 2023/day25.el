;; -*- lexical-binding: t; -*-

(load (expand-file-name "lisp/my-library.el" user-emacs-directory))
(load (expand-file-name "aoc.el" default-directory))
(load (expand-file-name "heap.el" default-directory))
(load (car (file-expand-wildcards
            (expand-file-name "elpa/queue-*/queue.el" user-emacs-directory))))

(defvar -network nil)
(defvar -nodes nil)
(defvar -graph nil)

;; xgs lmj
;; pgz hgk
;; gzr qnz



(with-puzzle "day25.dot"
  (setf -network (make-hash-table :test 'equal))
  (setf -nodes (make-hash-table :test 'equal))
  (setf -graph (make-hash-table :test 'equal))
  (loop for line in (string-lines (buffer-string))
        for (from . tos) = (string-split line "[>{} -]+" t)
        do (loop for to in tos
                 do (pushnew to (gethash from -network) :test #'equal)
                 do (pushnew from (gethash to -network) :test #'equal)
                 do (setf (gethash (cons from to) -graph) t)
                 do (setf (gethash (cons to from) -graph) t)
                 do (setf (gethash to -nodes) t))
        do (setf (gethash from -nodes) t))
  (list
   :network (hash-table-count -network)
   :graph (hash-table-count -graph)
   :nodes (hash-table-count -nodes)))

(with-puzzle "day25.dot"
 (let ((Q (make-queue)) seen)
   (cl-labels ((walk (node)
                 (setf (gethash node seen) t)
                 (let ((dsts (gethash node -network)))
                   ;; (pri dsts)
                   (loop for dst in dsts
                         unless (gethash dst seen)
                         do (queue-enqueue Q dst)))))

     (queue-enqueue Q "xgs")
     (setf seen (make-hash-table :test 'equal))
     (loop until (queue-empty Q) do (funcall #'walk (queue-dequeue Q)))
     (pri (hash-table-count seen))

     (setf seen (make-hash-table :test 'equal))
     (queue-enqueue Q "gzr")
     (loop until (queue-empty Q) do (funcall #'walk (queue-dequeue Q)))
     (pri (hash-table-count seen)))))

;; xgs
;; gzr

(* 729 732) ;; 533628
