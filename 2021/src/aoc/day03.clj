(ns aoc.day03
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(defn parse-data [data]
  (str/split-lines data))

(defn to-int [n]
  (read-string (format "2r%s" (apply str n))))

(defn gamma [report]
  (mapv (comp (comp first first)
              #(sort-by (comp count second) > %)
              #(group-by identity %))
        (mapv (partial map #(- (int %) (int \0)))
              (u/transpose report))))

(defn epsilon [report]
  (mapv #(bit-xor 1 %) (gamma report)))

(defn solve-1 [report]
  (* (to-int (apply str (epsilon report)))
     (to-int (apply str (gamma report)))))

(defn column [i report]
  (mapv #(nth % i) report))

(defn filter-by-column [default-choice cmp report i]
  (let [[[a af] [b bf]] (sort-by second cmp (frequencies (column i report)))
        x (if (= af bf) default-choice a)]
    (filterv #(= x (nth % i)) report)))

(defn rating [filter report]
  (let [n (count (first report))]
    (loop [numbers report
           i 0]
      (if (or (= 1 (count numbers)) (>= i n))
        (to-int (first numbers))
        (recur (filter numbers i)
               (inc i))))))

(defn oxygen-generator-rating [report]
  (rating (partial filter-by-column \1 >) report))

(defn co2-scrubber-rating [report]
  (rating (partial filter-by-column \0 <) report))

(defn life-support-rating [report]
  (* (oxygen-generator-rating report) (co2-scrubber-rating report)))

(defn solve-2 [report]
  (life-support-rating report))

(def example (parse-data "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(u/check solve-1 example 198)
(u/check solve-2 example 230)

(def data (parse-data (u/get-input 3)))

(u/check solve-1 data 1131506)
(u/check solve-2 data 7863147)
