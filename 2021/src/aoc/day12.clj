(ns aoc.day12
  (:require [aoc.utils :as u :refer [unless]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-data [data]
  (apply merge-with into
         (map (fn [[k v]]
                (let [kk (keyword k)
                      kv (keyword v)]
                  {kk [kv] kv [kk]}))
              (partition 2 (str/split data #"[^a-zA-Z]")))))

(def example-1
  (parse-data "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))

(def example-2 (parse-data "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
"))

(def example-3 (parse-data "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"))

(type :a)

(do
  (defn small? [^clojure.lang.Keyword k]
    (Character/isLowerCase (.charAt (name k) 0)))

  (defn visit [visited turn]
    (if (small? turn) (conj visited turn) visited))

  (defn count-paths [maze visited? from]
    (reduce + (map
               (fn [turn]
                 (cond (= turn :end) 1
                       (visited? turn) 0
                       :else (count-paths maze (visit visited? from) turn)))
               (remove visited? (maze from)))))

  (defn solve-1 [input]
    (count-paths input #{:start} :start))

  (u/check solve-1 example-1 10)
  (u/check solve-1 example-2 19)
  (u/check solve-1 example-3 226))

(do
  (defn count-paths-2 [maze visited? from twice]
    (reduce + (map
               (fn [turn]
                 (cond (= turn :end) 1
                       (and twice (visited? turn)) 0
                       :else (count-paths-2 maze (visit visited? turn) turn (or twice (visited? turn)))))
               (if twice
                 (remove visited? (maze from))
                 (remove #(= :start %) (maze from))))))

  (defn solve-2 [input]
    (count-paths-2 input #{:start} :start nil))

  (u/check solve-2 example-1 36)
  (u/check solve-2 example-2 103)
  (u/check solve-2 example-3 3509))

(do
  (def data (parse-data (u/get-input 12)))

  (u/check solve-1 data 3463)
  (u/check solve-2 data 91533))
