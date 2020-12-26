(defpackage #:day06
  (:use #:cl)
  (:import-from #:named-readtables #:in-readtable)
  (:import-from #:dop #:fn #:incfhash #:with-collecting #:counting #:summing #:doseq)
  (:import-from #:rutils #:% #:it #:-> #:->>))

(in-package #:day06)

(in-readtable :aoc)

(defparameter +groups+
  (->> (aoc:puzzle 6 2020)
       (cl-ppcre:split #?"\n\n")
       (mapcar #'str:lines)))
 ;; 6161

(with-collecting (total)
  (dolist (group +groups+)
    (with-collecting (answers)
      (doseq ((person group) (answer person))
        (counting answer :as answers))
      (summing (count (length group) (rtl:ht-vals answers)) :as total)))
  total) ;; 2971
