s;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'subr-x)

(defmacro with-output-buffer (name &rest body)
  (declare (indent 1))
  `(let ((standard-output (get-buffer-create ,name)))
     (with-current-buffer standard-output
       (erase-buffer))
     (print (progn ,@body))))

(defmacro with-puzzle (filepath &rest body)
  (declare (indent 1))
  `(with-output-buffer "*debug*"
     (with-current-buffer (find-file-noselect ,filepath)
       ,@body)))



;; day 10 part 1

;; | is a vertical pipe connecting north and south.
;; - is a horizontal pipe connecting east and west.
;; L is a 90-degree bend connecting north and east.
;; J is a 90-degree bend connecting north and west.
;; 7 is a 90-degree bend connecting south and west.
;; F is a 90-degree bend connecting south and east.

(with-puzzle "~/tmp/day10.txt"
  (let* ((input (buffer-string))
         (size (seq-position input ?\n))
         (maze (string-replace "\n" "" input))
         (len (length maze))
         (start (seq-position maze ?S))
         (visits (make-hash-table)))
    (cl-labels ((visit! (pos) (setf (gethash pos visits) t))
                (visitedp (pos) (gethash pos visits))
                (moves (pos)
                  (let ((r (/ pos size))
                        (c (mod pos size)))
                    (loop for (dir dr dc) in '((L 0 -1) (R 0 1) (U -1 0) (D 1 0))
                          for next = (+ pos (* dr size) dc)
                          when (and (< -1 (+ r dr) size)
                                    (< -1 (+ c dc) size)
                                    (not (visitedp next))
                                    (let ((from (aref maze pos))
                                          (to (aref maze next)))
                                      (pcase (list from dir to)
                                        ;; starting position
                                        (`(?S L ,(or ?- ?L ?F)) t)
                                        (`(?S R ,(or ?- ?J ?7)) t)
                                        (`(?S U ,(or ?| ?F ?7)) t)
                                        (`(?S D ,(or ?| ?J ?L)) t)
                                        ;;
                                        (`(?| U ,(or ?| ?F ?7)) t)
                                        (`(?| D ,(or ?| ?J ?L)) t)
                                        ;;
                                        (`(?- R ,(or ?- ?J ?7)) t)
                                        (`(?- L ,(or ?- ?F ?L)) t)
                                        ;;
                                        (`(,(or ?L ?J) U ,(or ?| ?F ?7)) t)
                                        (`(,(or ?L ?F) R ,(or ?- ?J ?7)) t)
                                        ;;
                                        (`(,(or ?J ?7) L ,(or ?- ?L ?F)) t)
                                        (`(,(or ?F ?7) D ,(or ?| ?L ?J)) t))))
                          collect (cons dir next)))))
      (visit! start)
      (pcase-exhaustive (moves start)
        (`((,_ . ,pos) (,_ . ,target))
         (visit! pos)
         (loop for i from 1 ;; below 100
               for ((dir . next) . _) = (moves pos) then (moves next)
               until (= next target)
               do (visit! next)
               ;; do (print (format "%d %d %s %c" i next dir (aref maze next)))
               finally (return (/ (+ 2 i) 2)))))))) ;; 6714




;; day 10 part 2

(with-puzzle "~/tmp/day10.txt"
  (let* ((input (buffer-string))
         (maze (string-replace "\n" "" input))
         (len (length maze))
         (size (seq-position input ?\n))
         (rows (/ len size))
         (start (seq-position maze ?S))
         (visits (make-hash-table)))
    (cl-labels ((visit! (pos) (setf (gethash pos visits) t))
                (visitedp (pos) (gethash pos visits))
                (moves (pos)
                  (let ((r (/ pos size))
                        (c (mod pos size)))
                    (loop for (dir dr dc) in '((L 0 -1) (R 0 1) (U -1 0) (D 1 0))
                          for next = (+ pos (* dr size) dc)
                          when (and (< -1 (+ r dr) rows)
                                    (< -1 (+ c dc) size)
                                    (not (visitedp next))
                                    (let ((from (aref maze pos))
                                          (to (aref maze next)))
                                      (pcase (list from dir to)
                                        ;; starting position
                                        (`(?S L ,(or ?- ?L ?F)) t)
                                        (`(?S R ,(or ?- ?J ?7)) t)
                                        (`(?S U ,(or ?| ?F ?7)) t)
                                        (`(?S D ,(or ?| ?J ?L)) t)
                                        ;;
                                        (`(?| U ,(or ?| ?F ?7)) t)
                                        (`(?| D ,(or ?| ?J ?L)) t)
                                        ;;
                                        (`(?- R ,(or ?- ?J ?7)) t)
                                        (`(?- L ,(or ?- ?F ?L)) t)
                                        ;;
                                        (`(,(or ?L ?J) U ,(or ?| ?F ?7)) t)
                                        (`(,(or ?L ?F) R ,(or ?- ?J ?7)) t)
                                        ;;
                                        (`(,(or ?J ?7) L ,(or ?- ?L ?F)) t)
                                        (`(,(or ?F ?7) D ,(or ?| ?L ?J)) t))))
                          collect (cons dir next)))))
      (visit! start)
      (pcase-exhaustive (moves start)
        (`((,_ . ,pos) (,_ . ,target))
         (visit! pos)
         (loop for i from 1 ;; below 100
               for ((dir . next) . _) = (moves pos) then (moves next)
               until (= next target)
               do (visit! next)
               ;; do (print (format "%d %d %s %c" i next dir (aref maze next)))
               finally (visit! target))))

      (setf (aref maze start) ?|) ;; :)

      (loop with inside? = nil
            with count = 0
            for r from 0 below rows
            do (setf inside? nil)
            do (loop for c from 0 below size
                     do (let* ((pos (+ (* r size) c))
                               (tile (aref maze pos))
                               (maze-tile? (visitedp pos)))
                          (pcase (list inside? maze-tile?)
                            (`(t nil)
                             (incf count))
                            (`(t t)
                             (setf inside? (and (memq tile '(?L ?J ?-)) t))) ;; ?- ?F ?L
                            (`(nil t)
                             (setf inside? (and (memq tile '(?7 ?F ?|)) t))))))
            finally (return count))))) ;; 429


