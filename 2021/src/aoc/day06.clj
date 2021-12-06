(ns aoc.day06
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-data [data]
  (u/parse-longs #"[,\n]" data))

(def example (parse-data "3,4,3,1,2"))

(defn step [fish-counts]
  (reduce-kv (fn [next cycle cnt]
            (if (zero? cycle)
              (merge-with + next {8 cnt} {6 cnt})
              (merge-with + next {(dec cycle) cnt})))
          {} fish-counts))

(defn solve-1
  ([fishes]
   (solve-1 80 fishes))
  ([cycles fishes]
   (loop [fishes (frequencies fishes) n cycles]
     (if (zero? n)
       (apply + (vals fishes))
       (recur (step fishes) (dec n))))))

(defn solve-2 [fishes]
  (solve-1 256 fishes))

(u/check solve-1 example 5934)
(u/check solve-2 example 26984457539)

(def data (parse-data (u/get-input 6)))

(u/check solve-1 data 371379)
(u/check solve-2 data 1674303997472)
