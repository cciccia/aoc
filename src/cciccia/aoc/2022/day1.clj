(ns cciccia.aoc.2022.day1
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [inputs]
  (->> inputs
       (map (partial apply +))
       (apply max)))

(defn part2
  [inputs]
  (->> inputs
       (map (partial apply +))
       sort
       reverse
       (take 3)
       (apply +)))

(comment
  (part1 (utils/load-edn-input "2022/day1.edn"))
  (part2 (utils/load-edn-input "2022/day1.edn")))
