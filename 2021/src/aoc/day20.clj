(ns aoc.day20
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :refer [permutations]]))

(defn parse-data [data]
  (let [[code image] (str/split data #"\n\n")
        lines (str/split-lines image)
        n (count lines)]
    {:code code :size [-1 (inc n)]
     :image (merge
             (into {} (for [x (range -1 (inc n))] [[x -1] \.]))
             (into {} (for [x (range -1 (inc n))] [[-1 x] \.]))
             (into {} (for [x (range -1 (inc n))] [[n x] \.]))
             (into {} (for [x (range -1 (inc n))] [[x n] \.]))
             (into {} (for [x (range n) y (range n)]
                             [[x y] (nth (lines y) x)])))}))

(def data (parse-data (u/get-input 20)))

(def example (parse-data "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"))

(defn adjacents [[x y]]
  (for [dy (range -1 2) dx (range -1 2)]
    [(+ x dx) (+ y dy)]))

(defn bol
  ([l] (bol l nil))
  ([l else] (case l \# 1 \. 0 else)))

(defn point->bits [code image p]
  (map #(bol (image %) (bol (nth code 0))) (adjacents p)))

(defn bits->number [bits]
  (second (reduce (fn [[p n] bit]
                    [(/ p 2) (+ n (* bit p))])
                  [256 0] bits)))

(defn point->index [code image p]
  (bits->number (point->bits code image p)))

(defn step [{:keys [size image code]}]
  (let [[k l] [(dec (size 0)) (inc (size 1))]]
    {:code code :size [k l]
     :image (into {} (for [x (range k l) y (range k l)]
                       [[x y] (nth code (point->index code image [x y]))]))}))

(defn pp [{:keys [size image]}]
  (str/join
   (flatten
    (let [[k l] size]
      (for [y (range k l) x (range k l)]
        (cons (or (image [x y]) \.)
              (when (= x (dec l))
                (list \newline))))))))

(print (str/join (pp (step example))))

;; 5026 too high
;; 4908 too low
(:code data)

(u/count-if #(= \# %) (vals (:image (step (step example)))))
(u/count-if #(= \# %) (vals (:image (step (step data)))))

(count (:image (step (step data))))

(count (:image (step (step (step (step (step data)))))))

(print (pp (step example-1)))
