(loop :for i :from 1 :to 25 :do
  (eval `(defpackage ,(make-symbol (format nil "ADVENT-OF-CODE-2019-DAY~D" i))
           (:use :cl :iterate :priority-queue)
           (:import-from :alexandria :compose :curry :rcurry)
           (:import-from :rutils :when-it :if-it :it))))
