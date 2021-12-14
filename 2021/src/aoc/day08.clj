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

(do
  (defn solve-1 [input]
    (reduce (fn [sum [_ output]]
              (+ sum (u/count-if #{2 4 3 7} (map count output))))
            0 input))

  (u/check solve-1 example 26))

(def numbers
  {1 "cf"
   4 "bcdf"
   7 "acf"
   8 "abcdefg"
   2 "acdeg" 3 "acdfg" 5 "abdfg"
   0 "abcefg" 6 "abdefg" 9 "abcdfg"})

(def decode
  (set/map-invert (map (fn [[k v]] [k (set v)]) numbers)))

(comment
  ;; Slow implementation

  (def dicts (mapv #(into {} (map vector "abcdefg" %)) (drop 1 (permutations "abcdefg"))))
  (def target (set (map set (vals numbers))))

  (defn translate [dict words]
    (mapv #(str/join (replace dict %)) words))

  (defn decode-line [[numbers answer]]
    (loop [[dict & rest] dicts]
      (if (= target (set (map set (translate dict numbers))))
        (let [[a b c d] (map decode (map set (translate dict answer)))]
          (+ (* 1000 a) (* 100 b) (* 10 c) d))
        (recur rest)))))

(defn bag [s]
  (into {} (map #(vector % 1) s)))

(defn diff1 [s1 s2]
  (first (set/difference s1 s2)))

(defn unhack [[input output]]
  (let [[one seven four l5 _ _ _ _ _ _] (map set (sort-by count input))
        {b 6 e 4 f 9} (set/map-invert (reduce (partial merge-with +) (map bag input)))
        c (diff1 one #{f})
        a (diff1 seven one)
        g (diff1 l5 (set/union four #{a e f b}))
        d (diff1 l5 (set/union one #{a e f b g}))
        tbl {a \a b \b c \c d \d e \e f \f g \g}]
    (let [[k l m n] (map (comp decode set #(replace tbl %)) output)]
      (+ (* 1000 k) (* 100 l) (* 10 m) n))))

(defn solve-2 [lines]
  (reduce + (map unhack lines)))

(u/check solve-2 example 61229)

(def data (parse-data (u/get-input 8)))

(u/check solve-1 data 543)
(u/check solve-2 data 994266)
