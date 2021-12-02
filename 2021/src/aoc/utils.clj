(ns aoc.utils
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-session []
  (str/trim (slurp "session.txt")))

(defn day-input-file-name [day]
  (format "inputs/day%02d.txt" day))

(defn day-input-exists? [day]
  (.exists (io/file (day-input-file-name day))))

(defn download-input [day]
  (:body
   (http/get (str "https://adventofcode.com/2021/day/" day "/input")
             {:cookies {"session" {:value (read-session)}}})))

(defn get-input [day]
  (let [fp (day-input-file-name day)]
    (if (day-input-exists? day)
      (slurp fp)
      (let [data (download-input day)]
        (spit fp data)
        data))))

(defmacro check [f input expected]
  `(let [actual# (~f ~input)
         fname# (format "%s/%s %s" (ns-name *ns*) (name '~f) '~input)]
     (if (= actual# ~expected)
       (println "✓" fname# "=" actual#)
       (println "-" fname# "=" actual# "but expected" ~expected))))

(defn transpose [coll]
  (apply mapv vector coll))

(defn parse-long [s]
  (Long/parseLong s))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse
  ([f s]
   (parse f #"(?m)\s+" (str/trim s)))
  ([f re s]
   (mapv f (str/split s re))))

(def parse-ints (partial parse parse-int))
(def parse-longs (partial parse parse-long))
