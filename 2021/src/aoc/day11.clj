(ns aoc.day11
  (:require [aoc.utils :as u :refer [unless]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn coordinates [w h xs]
  (into {} (for [x (range w)
                 y (range h)]
             [[x y] (xs (+ x (* y 10)))])))

(defn parse-data [data]
  (coordinates 10 10 (u/digits (str/join (str/split-lines data)))))

(def example
  (parse-data "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

(defn adjacents [[x y]]
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not (and (= dy 0) (= 0 dx)))]
    [(+ x dx) (+ y dy)]))

(defn energized? [o]
  (> o 9))

(do
  (defn step [octopi]
    (loop [octopi (u/map-vals inc octopi)
           flashed? #{}]
      (let [triggered (reduce-kv (fn [acc xy e]
                                   (cond (and (not (flashed? xy)) (energized? e)) (conj acc xy)
                                         :else acc))
                                 [] octopi)]
        (if (empty? triggered)
          (u/map-vals #(if (energized? %) 0 %) octopi)
          (recur (reduce (fn [m k] (update m k inc)) octopi
                         (filter #(and (not (flashed? %)) (octopi %))
                                 (mapcat adjacents triggered)))
                 (set/union flashed? (set triggered)))))))

  (defn solve-1 [octopi]
    (loop [o (step octopi)
           i 100
           total 0]
      (if (> i 0)
        (recur (step o) (dec i)
               (+ total (u/count-if zero? (vals o))))
        total)))

  (u/check solve-1 example 1656))

(do
  (defn solve-2 [octopi]
    (loop [o (step octopi)
           i 1]
      (if (= 100 (u/count-if zero? (vals o)))
        i
        (recur (step o) (inc i)))))

  (u/check solve-2 example 195))

(do
  (def data (parse-data (u/get-input 11)))

  (u/check solve-1 data 1688)
  (u/check solve-2 data 403))
