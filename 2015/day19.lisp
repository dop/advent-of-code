;; (ql:quickload '(:alexandria :rutils :str :cl-json))

(defpackage #:aoc201519
  (:use #:cl #:rutils)
  (:import-from #:alexandria #:curry #:rcurry #:compose)
  (:import-from #:rutils #:when-it #:if-it #:getsethash))

(in-package #:aoc201519)

(defparameter *replacements*
  '(("Al" . "ThF")
    ("Al" . "ThRnFAr")
    ("B" . "BCa")
    ("B" . "TiB")
    ("B" . "TiRnFAr")
    ("Ca" . "CaCa")
    ("Ca" . "PB")
    ("Ca" . "PRnFAr")
    ("Ca" . "SiRnFYFAr")
    ("Ca" . "SiRnMgAr")
    ("Ca" . "SiTh")
    ("F" . "CaF")
    ("F" . "PMg")
    ("F" . "SiAl")
    ("H" . "CRnAlAr")
    ("H" . "CRnFYFYFAr")
    ("H" . "CRnFYMgAr")
    ("H" . "CRnMgYFAr")
    ("H" . "HCa")
    ("H" . "NRnFYFAr")
    ("H" . "NRnMgAr")
    ("H" . "NTh")
    ("H" . "OB")
    ("H" . "ORnFAr")
    ("Mg" . "BF")
    ("Mg" . "TiMg")
    ("N" . "CRnFAr")
    ("N" . "HSi")
    ("O" . "CRnFYFAr")
    ("O" . "CRnMgAr")
    ("O" . "HP")
    ("O" . "NRnFAr")
    ("O" . "OTi")
    ("P" . "CaP")
    ("P" . "PTi")
    ("P" . "SiRnFAr")
    ("Si" . "CaSi")
    ("Th" . "ThCa")
    ("Ti" . "BP")
    ("Ti" . "TiTi")
    ("e" . "HF")
    ("e" . "NAl")
    ("e" . "OMg")))

(defparameter *molecule*
  "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr")

(defun replacement-table (replacements)
  (loop :with ht := (make-hash-table :test 'equal)
        :for (from . to) :in replacements :do
          (push to (gethash from ht))
        :finally
           (return ht)))

(defun positions (substring string)
  (loop :for i := (search substring string) :then (search substring string :start2 (1+ i))
        :while i
        :collect i))

(defun replace-at (string replacement start &optional end)
  (concatenate 'string
               (subseq string 0 start)
               replacement
               (subseq string (min end (length string)))))

(defun replacement-candidates (current candidate replacement)
  (loop
    :with len := (length candidate)
    :for position :in (positions candidate current)
    :collect (replace-at current replacement position (+ position len))))

(defun candidates-of-replacements (current candidate replacements)
  (apply #'concatenate 'list
         (loop :for replacement :in replacements
               :collect (replacement-candidates current candidate replacement))))

(defun candidates (current replacements-ht)
  (remove-duplicates
   (apply #'concatenate 'list
          (loop :for target :being :the :hash-key :of replacements-ht
                  :using (:hash-value replacements)
                :collect (candidates-of-replacements current target replacements)))
   :test #'equal))

(defun invert-pairs (pairs)
  (mapcar (lambda (x) (cons (cdr x) (car x))) pairs))

(defparameter *inverse-replacements*
  (invert-pairs *replacements*))

(defparameter *inverse-ht*
  (replacement-table *inverse-replacements*))

(defparameter *hits* (make-hash-table :test 'equal))
(defparameter *callcount* 0)

(defun solvable-p (candidate)
  (or (equal "e" candidate)
      (not (position #\e candidate))))

(defun solve (current replacement-table)
  (incf *callcount*)
  (if (and (position #\e current) (> (length current) 1))
    'infinity
    (or (gethash current *hits*)
        (setf (gethash current *hits*)
              (cond
                ((equal "e" current)
                 0)
                (t
                 (loop
                   :with best := 'infinity
                   :for candidate :in (remove-if-not #'solvable-p (candidates current replacement-table))
                   :for steps := (solve candidate replacement-table)
                   :when (numberp steps)
                     :do (let ((best? (1+ steps)))
                           (setf best
                                 (if (numberp best)
                                   (min best best?)
                                   best?)))
                   :finally
                      (return best))))))))

(loop
  :with molecule := (copy-seq *molecule*)
  :with count := 0
  :for i :from 0 :to 100 :do
    (loop
      :for (key . value) :in (sort (invert-pairs *replacements*) #'< :key (compose #'length #'cdr)) :do
        (incf count (length (positions key molecule)))
        (setf molecule (str:replace-all key value molecule)))
  :finally
     (return (list count molecule)))

(217 "CRnSiRnFYCaRnFArArFArAl")
