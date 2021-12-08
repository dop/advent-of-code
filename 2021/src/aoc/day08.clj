(ns aoc.day08
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :refer [permutations]]))

(defn parse-data [data]
  (mapv (comp #(vector (take 10 %) (drop 10 %))
              #(str/split % #"[|\s]+"))
        (str/split-lines data)))

(def example
  (parse-data "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(defn in? [coll el]
  (some #{el} coll))

(defn solve-1 [input]
  (reduce (fn [sum [_ output]]
            (+ sum (count (filter #(in? [2 4 3 7] (count %)) output))))
          0 input))

(def numbers
  {0 "abcefg" 6 "abdefg" 9 "abcdfg"
   2 "acdeg" 3 "acdfg" 5 "abdfg"
   1 "cf" 4 "bcdf" 7 "acf" 8 "abcdefg" })

(def dicts (mapv #(into {} (map vector "abcdefg" %)) (drop 1 (permutations "abcdefg"))))
(def target (set (map set (vals numbers))))
(def decode (set/map-invert (map (fn [[k v]] [k (set v)]) numbers)))

(defn translate [dict words]
  (mapv #(str/join (replace dict %)) words))

(defn decode-line [[numbers answer]]
  (loop [[dict & rest] dicts]
    (if (= target (set (map set (translate dict numbers))))
      (let [[a b c d] (map decode (map set (translate dict answer)))]
        (+ (* 1000 a) (* 100 b) (* 10 c) d))
      (recur rest))))

(defn solve-2 [lines]
  (reduce + (map decode-line lines)))

(u/check solve-1 example 26)
(u/check solve-2 example 61229)

(def data (parse-data (u/get-input 8)))

(u/check solve-1 data 543)
(u/check solve-2 data 994266)
