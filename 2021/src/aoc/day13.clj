(ns aoc.day13
  (:require [aoc.utils :as u :refer [unless]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-data [data]
  (let [[coords folds] (str/split data #"\n\n")]
    [(partition 2 (u/parse-longs #"[^\d]+" coords))
     (map (fn [[_ [axis] n]] [axis (u/parse-long n)])
          (re-seq #"fold along (.)=(\d+)" folds))]))

(def example
  (parse-data "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

(defn fold [x n]
  (if (< x n) x (- n (- x n))))

(defn fold-point [[x y] [axis n]]
  (case axis
    \y [x (fold y n)]
    \x [(fold x n) y]))

(defn pp [w h points]
  (str/join
   (flatten
    (for [y (range h)
          x (range w)]
      (cons (if (points [x y]) \# \space)
            (if (= x (dec w))
              (list \newline)
              nil))))))

(do
  (defn solve-1 [[points [f & folds]]]
    (count (set (for [p points] (fold-point p f)))))

  (u/check solve-1 example 17))

(defn solve-2 [[points folds]]
  (if-let [[f & rest] folds]
    (solve-2 [(for [p points] (fold-point p f)) rest])
    (print (pp (inc (apply max (map first points)))
               (inc (apply max (map second points)))
               (set points)))))

(do
  (def data (parse-data (u/get-input 13)))

  (u/check solve-1 data 785)
  (solve-2 data))
