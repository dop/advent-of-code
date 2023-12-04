(ns aoc.day19
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [clojure.math.combinatorics :refer [permutations]]))

(defn parse-data [data]
  (let [[_ & scanners] (str/split data #"--- scanner \d+ ---")]
    (mapv #(partition 3 (map u/parse-long (re-seq #"-?\d+" %))) scanners)))

(def data (parse-data (u/get-input 19)))

(defn subtract [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])

(defn translate [[dx dy dz] [x y z]]
  [(+ x dx) (+ y dy) (+ z dz)])

(defn rotax [[x y z]] [x z (- y)])
(defn rotay [[x y z]] [(- z) y x])
(defn rotaz [[x y z]] [(- y) x z])

(defn thrice [f x] (take 3 (drop 1 (iterate f x))))

(defn orientations [p]
  (->> (thrice rotax p)
       (mapcat #(into [%] (thrice rotay %)))
       (mapcat #(into [%] (thrice rotaz %)))))

(time (matches? (example 0) (example 1)))
(time (matches? (example 1) (example 4)))

(defn matches? [scanner0 scanner1]
  (first
   (for [beacons (set (u/transpose (map orientations scanner1)))
         :let [ts (for [b0 scanner0 b1 beacons] (subtract b1 b0))
               [t k] (first (filter #(> (second %) 11) (frequencies ts)))]
         :when t]
     [t beacons])))

(defn drop-at [i coll]
  (vec (concat (subvec coll 0 i)
               (subvec coll (inc i) (count coll)))))

(defn invert [[x y z]] [(- x) (- y) (- z)])

(defn solve-1 [input]
  (loop [[[curr dt] & queue] [[(input 0) [0 0 0]]]
         beacons (subvec input 1)
         region [curr]]
    (if (empty? beacons)
      (count (set (apply concat region)))
      (let [ms (for [i (range 0 (count beacons))
                     :let [[t bs] (matches? curr (beacons i))]
                     :when t]
                 [i bs (translate dt t)])]
        (recur (concat queue (mapv #(subvec % 1) ms))
               (vec (reduce (fn [bs i] (drop-at i bs)) beacons (sort > (map first ms))))
               (concat region (mapv (fn [[_ bs t]] (mapv #(translate (invert t) %) bs)) ms)))))))

(u/check solve-1 example 79)
(u/check solve-1 data 320)

(defn solve-2 [input]
  (loop [[[curr dt] & queue] [[(input 0) [0 0 0]]]
         beacons (subvec input 1)
         origins [[0 0 0]]]
    (if (empty? beacons)
      (reduce max (for [a origins
                        b origins]
                    (reduce + (map #(Math/abs ^long %) (subtract b a)))))
      (let [ms (for [i (range 0 (count beacons))
                     :let [[t bs] (matches? curr (beacons i))]
                     :when t]
                 [i bs (translate dt t)])]
        (assert (not (empty? ms)))
        (recur (concat queue (mapv #(subvec % 1) ms))
               (vec (reduce (fn [bs i] (drop-at i bs)) beacons (sort > (map first ms))))
               (concat origins (mapv (fn [[_ _ t]] t) ms)))))))

(u/check solve-2 example 3621)
(u/check solve-2 data 9655)
