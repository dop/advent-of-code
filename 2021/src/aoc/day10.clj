(ns aoc.day10
  (:require [aoc.utils :as u :refer [unless]]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn parse-data [data] (str/split-lines data))

(def example
  (parse-data "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(def invert (apply hash-map (vec "()[]{}<>><}{][)(")))

(do
  (defn check-line [line]
    (loop [[c & cs] line
           stack []]
      (match [c]
        [(:or \{ \( \[ \<)]
        (recur cs (conj stack c))

        [(:or \} \) \] \>)]
        (if (= c (invert (peek stack)))
          (recur cs (pop stack))
          [:unexpected c])

        :else [:incomplete stack])))

  (defn score [[typ val]]
    (if (= :unexpected typ)
      (case val \) 3 \] 57 \} 1197 \> 25137)
      0))

  (defn solve-1 [input]
    (reduce + (map (comp score check-line) input)))

  (u/check solve-1 example 26397))

(do
  (defn score-2 [cs]
    (reduce (fn [tot c]
              (+ (* tot 5)
                 (case c \) 1 \] 2 \} 3 \> 4)))
            0 cs))

  (defn completion [line]
    (match (check-line line)
      [:incomplete stack] (reverse (map invert stack))
      :else nil))

  (defn solve-2 [lines]
    (let [scores (->> (map (comp score-2 completion) lines)
                      (filter pos?)
                      sort)]
      (nth scores (Math/floor (/ (count scores) 2)))))

  (u/check solve-2 example 288957))

(def data (parse-data (u/get-input 10)))

(u/check solve-1 data 388713)
(u/check solve-2 data 3539961434)
