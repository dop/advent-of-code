(ns aoc.utils
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-session []
  (str/trim (slurp ".session")))

(defn day-input-file-name [year day]
  (format "inputs/%s/%02d.txt" year day))

(defn day-input-exists? [year day]
  (.exists (io/file (day-input-file-name year day))))

(defn download-input [year day]
  (:body
   (http/get (str "https://adventofcode.com/" year "/day/" day "/input")
             {:cookies {"session" {:value (read-session)}}})))

(defn get-input
  ([day]
   (get-input (.getYear (java.time.LocalDateTime/now)) day))
  ([year day]
   (let [fp (day-input-file-name year day)]
     (if (day-input-exists? year day)
       (slurp fp)
       (let [data (download-input year day)]
         (spit fp data)
         data)))))

(defmacro check [f input expected]
  `(let [start# (. System (currentTimeMillis))
         actual# (~f ~input)
         duration# (double (- (. System (currentTimeMillis)) start#))
         fname# (format "%s/%s %s" (ns-name *ns*) (name '~f) '~input)]
     (if (= actual# ~expected)
       (do (println "✓" fname# "=" actual# "in" duration# "msec.")
           (vector :ok actual#))
       (do (println "¬" fname# "≠" ~expected)
           (println "  got" actual#)
           (vector :fail actual#)))))

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

(defn rle [xs]
  (mapv #(vector (first %) (count %))
        (partition-by identity xs)))

(defn digit [c]
  (- (int c) (int \0)))

(defn digits [str]
  (mapv digit (vec str)))

(defmacro unless [test & body]
  `(when (not ~test)
     ~@body))

(defn none [pred coll]
  (not (some pred coll)))

(defn count-if
  ([pred coll]
   (count-if identity pred coll))
  ([key pred coll]
   (count (filter (comp pred key) coll))))

(defn map-kv [f m]
  (reduce-kv (fn [m k v]
               (assoc m k (f k v))) {} m))

(defn map-vals [f m]
  (map-kv (fn [_ v] (f v)) m))
