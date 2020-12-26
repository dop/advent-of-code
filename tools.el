;; -*- lexical-binding: t -*-

(defvar slime-watch-running-p nil)
(defvar slime-watch-interval 1)
(defvar slime-watch-session-count 0)

(defvar slime-watch-sexp nil)
(defvar slime-watch-sexp-package nil)
(defvar slime-watch-previous-output nil)

(defvar slime-watch-buffer-name "*Slime Watch*")

(defun slime-watch--get-buffer ()
  (or (get-buffer slime-watch-buffer-name)
      (with-current-buffer (get-buffer-create slime-watch-buffer-name)
        (so-long-mode)
        (buffer-disable-undo)
        (current-buffer))))

(defun slime-watch--show-and-enqueue (output)
  (when (not (equal output slime-watch-previous-output))
    (let ((buf (slime-watch--get-buffer)))
      (when (zerop slime-watch-session-count) (display-buffer buf))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (if slime-watch-running-p "" "Stopped ")
                (format-time-string "%F %T")
                "\n\n"
                (second output))
        (setq buffer-read-only t)
        (point-min)))
    (setq slime-watch-previous-output output))
  (when slime-watch-running-p
    (incf slime-watch-session-count)
    (slime-watch--eval-async-sexp-when-idle)))

(defun slime-watch--eval-async-sexp ()
  (slime-eval-async (list 'swank:eval-and-grab-output slime-watch-sexp)
    #'slime-watch--show-and-enqueue
    slime-watch-sexp-package))

(defun slime-watch--eval-async-sexp-when-idle ()
  (run-with-timer slime-watch-interval
                  nil
                  #'slime-watch--eval-async-sexp))

(defun slime-watch-sexp ()
  (interactive)
  (unless slime-watch-running-p
    (slime-watch--eval-async-sexp-when-idle))
  (setq slime-watch-running-p t
        slime-watch-session-count 0
        slime-watch-sexp (format "(handler-case %s (error (c) (print c)))"
                                 (slime-defun-at-point))
        slime-watch-sexp-package (slime-current-package)))

(defun slime-watch-stop ()
  (interactive)
  (setq slime-watch-running-p nil))

(define-key slime-editing-map (kbd "C-c w w") #'slime-watch-sexp)
(define-key slime-editing-map (kbd "C-c w s") #'slime-watch-stop)

(defun advent-of-code-day (year day)
  (interactive (list (read-string (format "Year: ") (format-time-string "%Y"))
                     (read-string (format "Day: "))))
  (eww (format "https://adventofcode.com/%s/day/%s" year day)))
