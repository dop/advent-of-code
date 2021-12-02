(ns aoc.day01
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-data [data]
  (mapv #(Integer/parseInt %) (str/split-lines data)))

(defn count-dips [depths]
  (reduce + (map (comp {false 0 true 1} <) depths (rest depths))))

(defn solve-1 [data]
  (count-dips data))

(defn solve-2 [data]
  (count-dips (map + data (drop 1 data) (drop 2 data))))

(def example [199 200 208 210 200 207 240 269 260 263])

(u/check solve-1 example 7)
(u/check solve-2 example 5)

(def data (parse-data (u/get-input 1)))

(u/check solve-1 data 1527)
(u/check solve-2 data 1575)
