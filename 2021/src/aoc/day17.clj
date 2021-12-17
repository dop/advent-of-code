(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn parse-bounds [data]
  (let [[tx1 tx2 ty1 ty2] (mapv u/parse-long (re-seq #"-?\d+" data))]
    {:x [tx1 tx2] :y [ty1 ty2]}))

(def example
  (parse-bounds "target area: x=20..30, y=-10..-5"))

(def data
  (parse-bounds (u/get-input 17)))

(defn step
  ([velocity bounds]
   (step 0 velocity [0 0] bounds))
  ([maxy [vx vy] [x y] {[tx1 tx2] :x [ty1 ty2] :y :as bounds}]
   (let [x' (+ x vx) y' (+ y vy)]
     (cond
       (and (<= tx1 x' tx2) (<= ty1 y' ty2))
       {:goal [maxy x' y']}

       (and (< x' tx2) (> y' ty1))
       (recur (max maxy y')
              [(max (dec vx) 0) (dec vy)]
              [(+ x vx) (+ y vy)]
              bounds)

       :else
       {:fail [x' y']}))))

(defn ok-velocities [vxs vys target]
  (for [vx vxs
        vy vys
        :let [result (step [vx vy] target)]
        :when (:goal result)]
    [vx vy (first (:goal result))]))

(defn highest-point [goals]
  (last (first (sort-by last > goals))))

(defn solve-1 [[[x1 x2] [y1 y2] input]]
  (let [rx (range x1 x2)
        ry (range y1 y2)]
    (highest-point (ok-velocities rx ry input))))

(u/check solve-1 [[-31 31] [-31 31] example] 45)
(u/check solve-1 [[22 297] [-97 97] data] 4656)

(defn solve-2 [[[x1 x2] [y1 y2] input]]
  (let [rx (range x1 x2)
        ry (range y1 y2)]
    (count (ok-velocities rx ry input))))

(u/check solve-2 [[-31 31] [-31 31] example] 112)
(u/check solve-2 [[22 297] [-97 97] data] 1908)
