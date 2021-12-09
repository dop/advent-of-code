(ns aoc.day09
  (:require [aoc.utils :as u :refer [unless]]
            [clojure.string :as str]))

(defn parse-data [data]
  (let [lines (str/split-lines data)]
    {:height (count lines)
     :width (count (first lines))
     :points (vec (flatten (mapv u/digits lines)))}))

(def example
  (parse-data "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"))

(do
  (defn inside? [{h :height w :width} x y]
    (and (< -1 x w) (< -1 y h)))

  (defn adjacent-points [input x y]
    (filterv #(apply inside? input %)
             [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

  (defn depth-at [{points :points w :width :as input} x y]
    (when (inside? input x y)
      (points (+ (* y w) x))))

  (defn lowest-points [{:keys [height width] :as input}]
    (for [y (range height)
          x (range width)
          :when (inside? input x y)
          :let [depth (depth-at input x y)]
          :when (u/none (fn [[x y]] (<= (depth-at input x y) depth))
                        (adjacent-points input x y))]
      [x y depth]))

  (defn solve-1 [input]
    (reduce (fn [sum [_ _ depth]] (+ sum (inc depth)))
            0 (lowest-points input)))

  (u/check solve-1 example 15))

(do
  (defn basin [{points :points :as input} x y]
    (loop [visited? #{}
           [[px py :as p] & rest] [[x y]]]
      (cond
        (and (nil? p) (empty? rest))
        (count (filter (fn [[x y]] (not (= 9 (depth-at input x y)))) visited?))

        (visited? p)
        (recur visited? rest)

        (= 9 (depth-at input px py))
        (recur (conj visited? p) rest)

        :else
        (recur (conj visited? p) (concat rest (adjacent-points input px py))))))

  (defn solve-2 [input]
    (reduce * (take 3 (sort > (map (fn [[x y _]] (basin input x y))
                                   (lowest-points input))))))

  (u/check solve-2 example 1134))

(def data (parse-data (u/get-input 9)))

(u/check solve-1 data 462)
(u/check solve-2 data 1397760)
