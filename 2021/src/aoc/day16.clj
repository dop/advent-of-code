(ns aoc.day16
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def hex->bits
  {\0  "0000"
   \1  "0001"
   \2  "0010"
   \3  "0011"
   \4  "0100"
   \5  "0101"
   \6  "0110"
   \7  "0111"
   \8  "1000"
   \9  "1001"
   \A  "1010"
   \B  "1011"
   \C  "1100"
   \D  "1101"
   \E  "1110"
   \F  "1111"})

(defn parse-data [data]
  (str/join (mapv hex->bits data)))

(defn read-binary [s]
  (read-string (format "2r%s" s)))

(assert (= 5 (read-binary "101")))

(defn parse [bits n f]
  (let [result (f (subs bits 0 n))]
    [result (subs bits n)]))

(assert (= [5 "0"] (parse "1010" 3 read-binary)))

(defn decode-number [data]
  (loop [input data
         acc 0
         continue true]
    (if continue
      (recur (subs input 5)
             (+ (bit-shift-left acc 4)
                (read-binary (subs input 1 5)))
             (= (first input) \1))
      [acc input])))

(def example-1 (parse-data "D2FE28"))

(assert (= [2021 "000"] (decode-number (subs example-1 6))))

(defn decode-length [end data decode]
  (loop [result [] rest (subs data 0 end)]
    (if (empty? rest)
      [result (subs data end)]
      (let [[packet rest] (decode rest)]
        (recur (conj result packet) rest)))))

(defn decode-times [n data decode]
  (loop [i n rest data result []]
    (if (zero? i)
      [result rest]
      (let [[res r] (decode rest)]
        (recur (dec i) r (conj result res))))))

(defn decode-operator [data decode]
  (if (= \0 (.charAt data 0))
    (let [[end rest] (parse (subs data 1) 15 read-binary)]
      (decode-length end rest decode))
    (let [[n rest] (parse (subs data 1) 11 read-binary)]
      (decode-times n rest decode))))

(defn decode [data]
  (let [[version data] (parse data 3 read-binary)
        [type    data] (parse data 3 read-binary)]
    (let [[result remaining]
          (if (= 4 type)
            (decode-number data)
            (decode-operator data decode))]
      [{:version version :type type :data result}
       remaining])))

(def example-2 (parse-data "38006F45291200"))
(def example-3 (parse-data "EE00D40C823060"))

(decode example-1)
(decode example-2)
(decode example-3)

(defn walk [f {:keys [type data] :as node}]
  (cons (f node)
        (when (not (= 4 type))
          (mapcat #(walk f %) data))))

(defn sum-versions [node]
  (reduce + (walk :version node)))

(defn solve-1 [data]
  (sum-versions (first (decode (parse-data data)))))

(do
  (u/check solve-1 "8A004A801A8002F478" 16)
  (u/check solve-1 "620080001611562C8802118E34" 12)
  (u/check solve-1 "C0015000016115A2E0802F182340" 23)
  (u/check solve-1 "A0016C880162017C3686B18A3D4780" 31)
  (u/check solve-1 (u/get-input 16) 957))

(defn eval1 [{:keys [type data]}]
  (case type
    0 (reduce + (map eval1 data))
    1 (reduce * (map eval1 data))
    2 (reduce min (map eval1 data))
    3 (reduce max (map eval1 data))
    4 data
    5 (if (reduce > (map eval1 data)) 1 0)
    6 (if (reduce < (map eval1 data)) 1 0)
    7 (if (reduce = (map eval1 data)) 1 0)))

(defn solve-2 [data]
  (eval1 (first (decode (parse-data data)))))

(do
  (u/check solve-2 "C200B40A82" 3)
  (u/check solve-2 "04005AC33890" 54)
  (u/check solve-2 "880086C3E88112" 7)
  (u/check solve-2 "CE00C43D881120" 9)
  (u/check solve-2 "D8005AC2A8F0" 1)
  (u/check solve-2 "F600BC2D8F" 0)
  (u/check solve-2 "9C005AC2F8F0" 0)
  (u/check solve-2 "9C0141080250320F1802104A08" 1)
  (u/check solve-2 (u/get-input 16) 744953223228))
