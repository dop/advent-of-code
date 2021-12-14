(ns aoc.day14
  (:require [aoc.utils :as u :refer [unless]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-data [data]
  (let [[template rules] (str/split data #"\n\n")]
    [(seq template) (into {} (map (fn [[k [v]]] [(seq k) v])
                                  (partition 2 (re-seq #"[A-Z]+" rules))))]))

(def example
  (parse-data "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))

(def insert
  (memoize
   (fn [rules i x y]
     (when (pos? i)
       (let [c (rules [x y])
             l (insert rules (dec i) x c)
             r (insert rules (dec i) c y)]
         (merge-with + {c 1} l r))))))

(defn solve [iterations [tpl rules]]
  (let [freqs (apply merge-with +
                     (frequencies tpl)
                     (map #(insert rules iterations %1 %2) tpl (drop 1 tpl)))
        [a b] ((juxt last first) (sort < (vals freqs)))]
    (- a b)))

(do
  (defn solve-1 [input]
    (solve 10 input))

  (u/check solve-1 example 1588))

(do
  (defn solve-2 [input]
    (solve 40 input))

  (u/check solve-2 example 2188189693529))

(do
  (def data (parse-data (u/get-input 14)))

  (u/check solve-1 data 2360)
  (u/check solve-2 data 2967977072188))
