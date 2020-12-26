(defpackage #:day04
  (:use #:cl)
  (:import-from #:named-readtables #:in-readtable)
  (:import-from #:trivia #:match)
  (:import-from #:trivia.ppcre #:ppcre)
  (:import-from #:cl-ppcre #:scan-to-strings)
  (:import-from #:rutils #:% #:it #:->> #:=> #:if-it #:when-it))

(in-package #:day04)

(in-readtable :aoc)

(defun parse-passport (text)
  (->> text
       (cl-ppcre:split #?"[\n :]+")
       (loop for (k v) on % by #'cddr
             collect (cons (intern (string-upcase k) 'keyword) v))
       (rtl:alist->ht)))

(defparameter *passports*
  (->> (aoc:puzzle 4 2020)
       (cl-ppcre:split #?"\n\n")
       (mapcar #'parse-passport)))

(defun valid-passport-p (passport)
  (loop for required-key in '(:byr :iyr :eyr :hgt :hcl :ecl :pid)
        always (rtl:in# required-key passport)))

(count-if #'valid-passport-p *passports*) ;; 182

(defparameter *field-validators*
  `((:byr ,#`(<= 1920 (parse-integer %) 2002))
    (:iyr ,#`(<= 2010 (parse-integer %) 2020))
    (:eyr ,#`(<= 2020 (parse-integer %) 2030))
    (:hgt ,#`(cond
              ((str:ends-with-p "in" %)
               (<= 59 (parse-integer % :junk-allowed t) 76))
              ((str:ends-with-p "cm" %)
               (<= 150 (parse-integer % :junk-allowed t) 193))))
    (:hcl ,#`(and (= 7 (length %)) (every #`(position % "abcdef0123456789") (subseq % 1))))
    (:ecl ,#`(member % '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'equal))
    (:pid ,#`(and (= 9 (length %)) (every #`(char<= #\0 % #\9) %)))))

(defun strict-valid-passport-p (passport)
  (loop for (key validp) in *field-validators*
        always (when-it (rtl:get# key passport)
                 (funcall validp it))))

(defun strict-valid-passport-p (passport)
  (match passport
    ((hash-table
      :byr byr :iyr iyr :eyr eyr :hgt hgt
      :hcl (ppcre "^#[a-f0-9]{6}$")
      :ecl (member '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'equal)
      :pid (ppcre "^[0-9]{9}$"))
     (and (<= 1920 (parse-integer byr) 2002)
          (<= 2010 (parse-integer iyr) 2020)
          (<= 2020 (parse-integer eyr) 2030)
          (match (rtl:2nd (scan-to-strings "^([0-9]+)(in|cm)$" hgt))
            ((vector (read number) "in")
             (<= 59 number 76))
            ((vector (read number) "cm")
             (<= 150 number 193)))))))

(count-if #'strict-valid-passport-p *passports*) ;; 109
