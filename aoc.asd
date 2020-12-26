(in-package #:asdf-user)

(defsystem #:aoc
  :depends-on (#:alexandria
               #:str
               #:rutils
               #:drakma
               #:should-test
               #:priority-queue
               #:cl-interpol
               #:clss
               #:plump
               #:trivia
               #:trivial-do
               #:trivia.ppcre)
  :pathname "."
  :components ((:file "readtable")
               (:file "web-client")
               (:file "utils")))
