(ns aoc.run
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn max-day []
  (match ((juxt #(.getYear %) #(.getMonthValue %) #(.getDayOfMonth %)) (java.time.LocalDate/now))
    [2021 12 d] d _ 25))

(defmacro run-all []
  (let [days (if (empty? *command-line-args*)
               (range 1 (inc (max-day)))
               (map #(Integer/parseInt %) *command-line-args*))]
    `(do ~@(for [day days]
             `(try
                (require '[~(symbol (format "aoc.day%02d" day))])
                (catch Exception e#
                  (println (format "- failed day %d solution: %s" ~day (:cause (Throwable->map e#))))))))))

(defn -main []
  (time (run-all)))
