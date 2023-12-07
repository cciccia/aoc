(ns cciccia.aoc.y2023.day7
  (:require [cciccia.aoc.utils :as utils]
            [clojure.math.combinatorics :as combo]))

(def card-values
  {"A" 14
   "K" 13
   "Q" 12
   "J" 11
   "T" 10
   "9" 9
   "8" 8
   "7" 7
   "6" 6
   "5" 5
   "4" 4
   "3" 3
   "2" 2
   "1" 1
   "0" 0})

(def hand-values
  {[5] 6
   [4 1] 5
   [3 2] 4
   [3 1 1] 3
   [2 2 1] 2
   [2 1 1 1] 1
   [1 1 1 1 1] 0})

(defn expand-for-jokers
  [h]
  (or (apply combo/cartesian-product (map
                                       (fn [i]
                                         (if (= \0 (get h i))
                                           (disj (set h) \0)
                                           [(get h i)]))
                                       (range 5)))
      ['(\1 \1 \1 \1 \1)]))


(defn tiebreaker-value
  [h]
  (apply + (map-indexed (fn [i v]
                          (* (int (Math/pow 15 i)) (card-values (str v))))
                        (reverse h))))

(defn hand-value
  [h]
  (+ (* (int (Math/pow 15 5))
        (apply max
               (map
                 #(hand-values (sort > (vals (frequencies %))))
                 (expand-for-jokers h))))
     (tiebreaker-value h)))

(defn part1
  [input]
  (->> input
       (sort-by (comp hand-value first))
       (map-indexed (fn [i v]
                      (* (inc i) (second v))))
       (apply +)))

(comment
  (part1 (utils/load-edn-input "2023/day7-jokers.edn")))

