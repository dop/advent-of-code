(ns aoc.day18
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn parse-data [data]
  (mapv read-string (str/split-lines data)))

(def data
  (parse-data (u/get-input 18)))

(defn magnitude [node]
  (if (vector? node)
    (let [[l r] node]
      (+ (* 3 (magnitude l)) (* 2 (magnitude r))))
    node))

(do
  (assert (= 143 (magnitude [[1,2],[[3,4],5]])))
  (assert (= 1384 (magnitude [[[[0,7],4],[[7,8],[6,0]]],[8,1]])))
  (assert (= 445 (magnitude [[[[1,1],[2,2]],[3,3]],[4,4]])))
  (assert (= 791 (magnitude [[[[3,0],[5,3]],[4,4]],[5,5]])))
  (assert (= 1137 (magnitude [[[[5,0],[7,4]],[5,5]],[6,6]])))
  (assert (= 3488 (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]))))

(defn add-left [node n]
  (if (zero? n)
    node
    (match node
      [a b] [(add-left a n) b]
      _     (+ node n))))

(defn add-right [node n]
  (if (zero? n)
    node
    (match node
      [a b] [a (add-right b n)]
      _     (+ node n))))

(defn explode
  ([tree]
   (match (explode 0 tree)
     [:explode _ t] [:explode t]
     _              tree))
  ([depth node]
   (match [depth node]
     [4 [a b]] [:explode [a b] 0]

     [_ [a b]]
     (match (explode (inc depth) a)
       [:explode [al ar] a']
       [:explode [al 0] [a' (add-left b ar)]]
       _
       (match (explode (inc depth) b)
         [:explode [bl br] b']
         [:explode [0 br] [(add-right a bl) b']]
         _
         [a b]))

     [_ _] node)))

(defn div [n d] (Math/floorDiv n d))

(defn split [tree]
  (match tree
    [a b] ; branch
    (match (split a)
      [:split a'] ; left split
      [:split [a' b]]

      _ (match (split b)
          [:split b'] ; right split
          [:split [a b']]

          _ [a b]))

    n ; leaf
    (if (< n 10)
      n
      [:split [(div n 2) (div (inc n) 2)]])))

(defn reduce! [tree]
  (match (explode tree)
    [:explode tree'] (reduce! tree')

    _ (match (split tree)
        [:split tree'] (reduce! tree')
        _              tree)))

(do
  (assert (= (explode [[1,2],[[3,4],5]])
             [[1,2],[[3,4],5]]))

  (assert (= (explode [[[[[9,8],1],2],3],4])
             [:explode [[[[0 9] 2] 3] 4]]))

  (assert (= (explode [7,[6,[5,[4,[3,2]]]]])
             [:explode [7 [6 [5 [7 0]]]]]))

  (assert (= (explode [[6,[5,[4,[3,2]]]],1])
             [:explode [[6 [5 [7 0]]] 3]]))

  (assert (= (reduce! [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
             [[3 [2 [8 0]]] [9 [5 [7 0]]]])))

(assert (= (reduce! [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])
           [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]))

(def example-1 [[1,1] [2,2] [3,3] [4,4]])
(assert (= (reduce (fn [a b] (reduce! [a b])) example-1)
           [[[[1 1] [2 2]] [3 3]] [4 4]]))

(def example-2 [[1,1] [2,2] [3,3] [4,4] [5 5]])
(assert (= (reduce (fn [a b] (reduce! [a b])) example-2)
           [[[[3 0] [5 3]] [4 4]] [5 5]]))

(def example-3 [[1,1] [2,2] [3,3] [4,4] [5,5] [6,6]])
(assert (= (reduce (fn [a b] (reduce! [a b])) example-3)
           [[[[5 0] [7 4]] [5 5]] [6 6]]))

(def example-4
  [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
   [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
   [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
   [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
   [7,[5,[[3,8],[1,4]]]]
   [[2,[2,2]],[8,[8,1]]]
   [2,9]
   [1,[[[9,3],9],[[9,0],[0,7]]]]
   [[[5,[7,4]],7],1]
   [[[[4,2],2],6],[8,7]]])

(assert
 (= (reduce! [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]] [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]])
    [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]))

(def example
  [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
   [[[5,[2,8]],4],[5,[[9,9],0]]]
   [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
   [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
   [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
   [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
   [[[[5,4],[7,7]],8],[[8,3],8]]
   [[9,3],[[9,9],[6,[4,9]]]]
   [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
   [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])

(defn solve-1 [input]
  (magnitude (reduce (fn [a b] (reduce! [a b])) input)))

(u/check solve-1 example 4140)
(u/check solve-1 data 4347)

(defn solve-2 [input]
  (reduce max (for [x input y input :when (not (= x y))] (solve-1 [x y]))))

(u/check solve-2 example 3993)
(u/check solve-2 data 4721)
