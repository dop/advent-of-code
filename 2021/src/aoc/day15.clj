(ns aoc.day15
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map-by]]))

(defn parse-data [data]
  (let [w (str/index-of data \newline)]
    [w w (u/coordinates w w (mapv u/parse-long (re-seq #"\d" data)))]))

(def example
  (parse-data "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"))

(defn adjacents [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn dijkstra [risk-levels target]
  (loop [low-risks (priority-map-by < [0 0] 0)
         visited {}]
    (let [[point risk] (peek low-risks)]
      (if (= point target)
        risk
        (let [nearby (remove #(or (not (risk-levels %)) (visited %)) (adjacents point))
              updated-risks (reduce (fn [m p]
                                      (assoc m p (min (+ risk (risk-levels p))
                                                      (or (m p) Long/MAX_VALUE))))
                                    (pop low-risks)
                                    nearby)]
          (recur updated-risks (conj visited [point risk])))))))

(defn enlarge [times [w h levels]]
  [(* w times)
   (* h times)
   (into {} (for [i (range times)
                  j (range times)
                  x (range w)
                  y (range h)]
              [[(+ x (* i w)) (+ y (* j h))]
               (inc (rem (dec (+ i j (levels [x y]))) 9))]))])

(def data (parse-data (u/get-input 15)))

(do
  (defn solve [[w h levels]]
    (dijkstra levels [(dec w) (dec h)]))

  (u/check solve example 40)
  (u/check solve (enlarge 5 example) 315)
  (u/check solve data 472)
  (u/check solve (enlarge 5 data) 2851))
