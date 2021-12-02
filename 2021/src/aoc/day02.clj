(ns aoc.day02
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-data [data]
  (mapv (fn [[dir amount]] [(keyword dir) (Integer/parseInt amount)])
        (partition 2 (str/split data #"(?m)[ \n\r]"))))

(defn solve-1 [moves]
  (apply * (reduce (fn [[x y] [dir amount]]
                     (case dir
                       :forward [(+ x amount) y]
                       :up [x (- y amount)]
                       :down [x (+ y amount)]))
                   [0 0] moves)))

(defn solve-2 [moves]
  (let [[x y]
        (reduce (fn [[x y aim] [dir amount]]
                  (case dir
                    :forward [(+ x amount) (+ y (* aim amount)) aim]
                    :up [x y (- aim amount)]
                    :down [x y (+ aim amount)]))
                [0 0 0] moves)]
    (* x y)))

(def example (parse-data "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(u/check solve-1 example 150)
(u/check solve-2 example 900)

(def data (parse-data (u/get-input 2)))

(u/check solve-1 data 2027977)
(u/check solve-2 data 1903644897)
