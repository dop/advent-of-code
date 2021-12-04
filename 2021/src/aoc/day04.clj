(ns aoc.day04
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-data [data]
  (let [[nums & boards] (str/split data #"\n\n")]
    {:numbers (u/parse-ints #"," nums)
     :boards (mapv (comp u/parse-ints str/trim) boards)}))

(def size 5)
(def rows (vec (partition size (range (* size size)))))
(def columns (apply mapv vector rows))
(def rows-and-cols (concat rows columns))

(defn struck? [board cells]
  (every? nil? (map board cells)))

(defn won? [board]
  (if (some (partial struck? board) rows-and-cols)
    board))

(def example
  (parse-data "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"))

(defn solve-1 [{:keys [numbers boards]}]
  (let [n (first numbers)
        updated (mapv (partial replace {n nil}) boards)
        winning (some won? updated)]
    (if winning
      (* n (apply + (remove nil? winning)))
      (solve-1 {:numbers (rest numbers) :boards updated}))))

(defn solve-2 [{:keys [numbers boards]}]
  (let [n (first numbers)
        [b & bs :as all] (mapv (partial replace {n nil}) boards)]
    (if (and (not bs) (won? b))
      (* n (apply + (remove nil? b)))
      (solve-2 {:numbers (rest numbers) :boards (remove won? all)}))))

(u/check solve-1 example 4512)
(u/check solve-2 example 1924)

(def data (parse-data (u/get-input 4)))

(u/check solve-1 data 2745)
(u/check solve-2 data 6594)
