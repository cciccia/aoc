(ns cciccia.aoc.y2022.day4
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]))

(defn part1
  [input]
  (->>
    input
    (filter
      (fn [[s1 e1 s2 e2]]
        (let [set1 (set (range s1 (inc e1)))
              set2 (set (range s2 (inc e2)))
              both (set/union set1 set2)]
          (or (= both set1) (= both set2)))))
    count))

(defn part2
  [input]
  (->>
    input
    (filter
      (fn [[s1 e1 s2 e2]]
        (let [set1 (set (range s1 (inc e1)))
              set2 (set (range s2 (inc e2)))]
          (seq (set/intersection set1 set2)))))
    count))

(comment
  (part2 (utils/load-edn-input "2022/day4.edn")))
