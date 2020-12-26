(defpackage #:aoc
  (:use :cl)
  (:import-from #:alexandria
                #:read-file-into-string
                #:write-string-into-file)
  (:import-from #:named-readtables #:in-readtable)
  (:import-from #:rutils #:it #:->> #:%)
  (:export #:puzzle #:submit))

(in-package #:aoc)

(in-readtable :aoc)

(defparameter *session-file*
  #P"/Users/donatasp/projects/advent-of-code/session.txt")

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun current-day ()
  (nth-value 3 (decode-universal-time (get-universal-time))))

(defun ensure-session ()
  (if (probe-file *session-file*)
      (str:trim (read-file-into-string *session-file*))
      (write-string-into-file
       (progn
         (format t "~%Session Id: ")
         (read-line))
       *session-file*)))

(defun make-session-cookie ()
  (let ((cookie (make-instance 'drakma:cookie
                               :domain "adventofcode.com"
                               :name "session"
                               :value (ensure-session))))
    (make-instance 'drakma:cookie-jar :cookies (list cookie))))

(defun download-puzzle-input (&optional (day (current-day)) (year (current-year)))
  (drakma:http-request (format nil "https://adventofcode.com/~d/day/~d/input" year day)
                       :cookie-jar (make-session-cookie)))

(defun ensure-puzzle-input (&optional (day (current-day)) (year (current-year)))
  (let ((puzzle-file (format nil "~d/day~2,'0d.txt" year day)))
    (if (probe-file puzzle-file)
        (read-file-into-string puzzle-file)
        (write-string-into-file (download-puzzle-input day year)
                                (ensure-directories-exist puzzle-file)))))

(defun puzzle (&optional (day (current-day)) (year (current-year)))
  (ensure-puzzle-input day year))

(defun parse-submit-parameters (html)
  (->> (plump:parse html)
       (clss:select "input")
       (map 'list #'plump:attributes)
       (remove-if #`(or (null (gethash "name" %))
                        (string= "answer" (gethash "name" %))))
       (mapcar #`(cons (gethash "name" %) (gethash "value" %)))))

(defun parse-main-text (html)
  (->> html
       plump:parse
       (clss:select "main")
       (remove-if #`(string= "script" (plump:tag-name %)))
       (map 'list #'plump:text)
       (str:join "")))

(defun submit-answer (answer &optional (day (current-day)) (year (current-year)))
  (let* ((cookies (make-session-cookie))
         (html (drakma:http-request (format nil "https://adventofcode.com/~d/day/~d" year day) :cookie-jar cookies))
         (parameters (parse-submit-parameters html)))
    (parse-main-text
     (drakma:http-request (format nil "https://adventofcode.com/~d/day/~d/answer" year day)
                          :method :post
                          :parameters `(("answer" . ,answer) ,@parameters)
                          :cookie-jar cookies))))

(defun submit (answer &optional (day (current-day)) (year (current-year)))
  (submit-answer (format nil "~a" answer) day year))
