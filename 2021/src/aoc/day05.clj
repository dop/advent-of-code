(ns aoc.day05
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-data [data]
  (partition 4 (map u/parse-long (str/split data #"[^\d]+"))))

(def example (parse-data "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(defn straight? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn line-points [[x1 y1 x2 y2]]
  (cons [x1 y1]
        (let [x (+ x1 (compare x2 x1))
              y (+ y1 (compare y2 y1))]
          (when (not (and (= x1 x2) (= y1 y2)))
            (line-points [x y x2 y2])))))

(defn count-intersecting-points [lines]
  (->> (mapv line-points lines)
       (apply concat)
       (frequencies)
       (vals)
       (filter #(< 1 %))
       (count)))

(defn solve-1 [lines]
  (count-intersecting-points (filter straight? lines)))

(defn solve-2 [lines]
  (count-intersecting-points lines))

(u/check solve-1 example 5)
(u/check solve-2 example 12)

(def data (parse-data (u/get-input 5)))

(u/check solve-1 data 6856)
(u/check solve-2 data 20666)
