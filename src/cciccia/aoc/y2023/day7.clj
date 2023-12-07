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

(defn eval-with-jokers
  [f h]
  (some
    f
    (or (apply combo/cartesian-product (map
                                         (fn [i]
                                           (if (= \0 (get h i))
                                             (disj (set h) \0)
                                             [(get h i)]))
                                         (range 5)))
        [["11111"]])))


(defn tiebreaker-value
  [h]
  (apply + (map-indexed (fn [i v]
                          (* (int (Math/pow 15 i)) (card-values (str v))))
                        (reverse h))))

(defn five-of-a-kind?
  [h]
  (apply = h))

(defn four-of-a-kind?
  [h]
  (some (fn [s]
          (apply = s))
        (combo/combinations h 4)))

(defn full-house?
  [h]
  (some
    (fn [[s1 s2]]
      (and (< 1 (count s1) 4)
           (apply = s1)
           (apply = s2)))
    (combo/partitions h :min 2 :max 2)))

(defn three-of-a-kind?
  [h]
  (some (fn [s]
          (apply = s))
        (combo/combinations h 3)))

(defn two-pair?
  [h]
  (some
    (fn [[s1 s2]]
      (and (< 1 (count s1) 4)
           (< (count (set s1)) (count s1))
           (< (count (set s2)) (count s2))))
    (combo/partitions h :min 2 :max 2)))

(defn pair?
  [h]
  (some (fn [s]
          (apply = s))
        (combo/combinations h 2)))

(defn hand-value
  [h]
  (+ (* (int (Math/pow 15 5))
        (cond
          (eval-with-jokers five-of-a-kind? h)
          6

          (eval-with-jokers four-of-a-kind? h)
          5

          (eval-with-jokers full-house? h)
          4

          (eval-with-jokers three-of-a-kind? h)
          3

          (eval-with-jokers two-pair? h)
          2

          (eval-with-jokers pair? h)
          1

          :else
          0))
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

