(defun aoc-example-string ()
  (save-excursion
    (search-forward ";; example")
    (next-line)
    (beginning-of-line)
    (let ((start (point)))
      (if (search-forward ";; input" nil t)
          (progn
            (previous-line)
            (beginning-of-line))
        (end-of-buffer))
      (string-trim
       (buffer-substring-no-properties start (point))))))

(defun aoc-input-string ()
  (save-excursion
    (search-forward ";; input")
    (next-line)
    (beginning-of-line)
    (let ((start (point)))
      (end-of-buffer)
      (string-trim
       (buffer-substring-no-properties start (point))))))

(provide 'aoc)
