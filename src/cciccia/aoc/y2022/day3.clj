(ns cciccia.aoc.y2022.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [cciccia.aoc.utils :as utils]))

(defn chr->priority
  [chr]
  (if (= (str/lower-case chr) (str chr))
    (- (int chr) 96)
    (- (int chr) 38)))

(defn part1
  [sacks]
  (->> sacks
       (map (fn [sack]
              (let [size (int (/ (count sack) 2))]
                (-> (set/intersection
                      (set (take size sack))
                      (set (drop size sack)))
                    first
                    chr->priority))))
       (apply +)))

(defn part2
  [sacks]
  (->> sacks
       (partition 3)
       (map (fn [[s1 s2 s3]]
              (-> (set/intersection (set s1) (set s2) (set s3))
                  first
                  chr->priority)))
       (apply +)))

(comment
  (part2 (utils/lined-spaced-input->str "2022/day3.txt")))
