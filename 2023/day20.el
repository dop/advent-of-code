;; -*- lexical-binding: t; -*-

(load (concat default-directory "aoc.el"))
(load (concat default-directory "heap.el"))

(add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/elpa/queue-*" t)))
(require 'queue)



(with-puzzle (:in "day20.txt")
  (cl-labels ((module-name (name)
                (string-trim-left name "[&%]")))
    (loop for block in (string-split (buffer-string) "\n\n" t)
          do (let* ((conns-alist
                     (mapcar (lambda (conn)
                               (string-split conn "[ >,-]" t))
                             (string-lines block)))
                    (conns ;; hashmap of module -> modules connections
                     (alist-hash-table
                      (mapcar (lambda (conn) (cons (module-name (car conn))
                                              (cdr conn)))
                              conns-alist)
                      :test 'equal))
                    (modules ;; state of each module
                     (alist-hash-table
                      (mapcar (lambda (conn)
                                (let ((name (car conn)))
                                  (cons (module-name name)
                                        (case (elt name 0)
                                          (?% (cons 'flip nil))
                                          (?& (cons 'conj
                                                    (alist-hash-table
                                                     (loop for (src . dsts) in conns-alist
                                                           when (seq-contains-p dsts (module-name name) #'equal)
                                                           collect (cons (module-name src) 'low))
                                                     :test 'equal)))))))
                              conns-alist)
                      :test 'equal))
                    (Q (make-queue))
                    (lows 0)
                    (highs 0)
                    (clicks 0))
               ;; (pr "input:\n%s\n" block)
               ;; (pr "map:\n%s" (pp-to-string conns-alist))
               ;; (pr "states:\n%s\n" (pp-to-string modules))

               (cl-labels ((send (src dsts pulse)
                             (if (eq pulse 'low) (incf lows (length dsts)) (incf highs (length dsts)))
                             (loop for dst in dsts do (queue-enqueue Q (list src dst pulse)))))
                 (loop for i from 1 to 1000000
                       do (send "button" (list "broadcaster") 'low)
                       do (loop until (queue-empty Q)
                                for (src dst pulse) = (queue-dequeue Q)
                                ;; do (pr "%s -%s-> %s" src pulse dst)
                                ;; do (pr "%s: %d" dst (case pulse (low 0) (t 1)))
                                do (progn
                                     ;; (pr "%d" i)

                                     ;; (when (and (equal src "rx") (eq pulse 'low))
                                     ;;   (pr "clicks: %d" i)
                                     ;;   (return))

                                     (let* ((inputs (hash-table-alist (cdr (gethash "kz" modules))))
                                            (highs (seq-filter (lambda (pair) (eq 'high (cdr pair))) inputs)))
                                       ;; (pr "%s" (hash-table-values inputs))
                                       (when highs
                                         (pr "%6d. %s" i highs)))

                                     (let ((nexts (gethash dst conns))
                                           (module (gethash dst modules)))
                                       ;; (pr "%s" module)
                                       (case (car module)
                                         (flip
                                          (let ((on (cdr module)))
                                            (when (eq 'low pulse)
                                              (send dst nexts (if on 'low 'high))
                                              (setf (cdr module) (not on)))))
                                         (conj
                                          (setf (gethash src (cdr module)) pulse)
                                          (send dst nexts (if (seq-every-p (lambda (p) (eq p 'high))
                                                                           (hash-table-values (cdr module)))
                                                              'low
                                                            'high)))
                                         (t
                                          (send dst nexts pulse))))))
                       ;; after
                       do (loop for m in '("kz")
                                do (let* ((inputs (hash-table-alist (cdr (gethash m modules))))
                                          (highs (seq-filter (lambda (pair) (eq 'high (cdr pair))) inputs)))
                                     ;; (pr "%s" (hash-table-values inputs))
                                     (when highs
                                       (pr "%6d. %s %s" i m highs))))))))))

(* 3739 3797 3919 4003) ;; 222718819437131
