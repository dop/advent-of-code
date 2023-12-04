;; -*- lexical-binding: t; -*-

(defun advent-of-code-eww (year day)
  (interactive (list (read-string (format "Year: ") (format-time-string "%Y"))
                     (read-string (format "Day: ") (format-time-string "%d"))))
  (eww (format "https://adventofcode.com/%s/day/%s" year day)))

(defun advent-of-code-set-session-cookie (session)
  (setf (cdr (assoc ".adventofcode.com" url-cookie-secure-storage))
        (list (url-cookie-create
               :name "session"
               :value session
               :expires (format-time-string "%a %b %d %H:%M:%S %Y GMT" (time-add nil (* 30 24 60 60)))
               :domain ".adventofcode.com"
               :localpart "/"
               :secure t))))

(provide 'advent-of-code)
