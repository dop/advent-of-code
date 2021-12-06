(ns aoc.day06
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn parse-data [data]
  (u/parse-longs #"[,\n]" data))

(def example (parse-data "3,4,3,1,2"))

(defn rle [xs]
  (mapv #(vector (first %) (count %))
        (partition-by identity xs)))

(defn step [rle-fishes]
  (loop [fishes rle-fishes
         new 0
         next []]
    (if (zero? (count fishes))
      (if (zero? new)
        next
        (conj next [8 new]))
      (let [[cycle cnt] (first fishes)
            spawn? (zero? cycle)]
        (recur (rest fishes)
               (if spawn? (+ cnt new) new)
               (conj next (if spawn? [6 cnt] [(dec cycle) cnt])))))))

(defn solve-1
  ([fishes]
   (solve-1 80 fishes))
  ([cycles fishes]
   (loop [fishes (rle fishes) n cycles]
     (if (zero? n)
       (apply + (map second fishes))
       (recur (step fishes) (dec n))))))

(defn solve-2 [fishes]
  (solve-1 256 fishes))

(u/check solve-1 example 5934)
(u/check solve-2 example 26984457539)

(def data (parse-data (u/get-input 6)))

(u/check solve-1 data 371379)
(u/check solve-2 data 1674303997472)
