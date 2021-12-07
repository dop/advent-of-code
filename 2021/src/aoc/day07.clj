(ns aoc.day07
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-data [data]
  (u/parse-longs #"[,\n]" data))

(def example (parse-data "16,1,2,0,4,2,7,1,2,14"))

(defn fuel [costfn target crabs]
  (reduce-kv (fn [sum pos cnt] (+ sum (* cnt (costfn pos target)))) 0 crabs))

(defn distance [a b]
  (Math/abs ^long (- a b)))

(defn solve-1
  ([positions]
   (solve-1 distance positions))
  ([costfn positions]
   (let [crabs (frequencies positions)]
     (apply min
            (for [i (range (inc (apply max positions)))]
              (fuel costfn i crabs))))))

(defn sum-1-n [n]
  (/ (* n (+ 1 n)) 2))

(defn solve-2 [positions]
  (solve-1 (comp sum-1-n distance) positions))

(u/check solve-1 example 37)
(u/check solve-2 example 168)

(def data (parse-data (u/get-input 7)))

(u/check solve-1 data 339321)
(u/check solve-2 data 95476244)
