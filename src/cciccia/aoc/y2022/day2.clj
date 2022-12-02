(ns cciccia.aoc.y2022.day2
  (:require [cciccia.aoc.utils :as utils]))

(def outcome-map
  {["A" "X"] 3
   ["A" "Y"] 6
   ["A" "Z"] 0
   ["B" "X"] 0
   ["B" "Y"] 3
   ["B" "Z"] 6
   ["C" "X"] 6
   ["C" "Y"] 0
   ["C" "Z"] 3})

(def play-map
  {"A" 1
   "B" 2
   "C" 3
   "X" 1
   "Y" 2
   "Z" 3})

(def aggregate-map
  {["A" "X"] 3
   ["A" "Y"] 4
   ["A" "Z"] 8
   ["B" "X"] 1
   ["B" "Y"] 5
   ["B" "Z"] 9
   ["C" "X"] 2
   ["C" "Y"] 6
   ["C" "Z"] 7})

(defn part1
  [inputs]
  (->> inputs
       (map (fn [[theirs mine]]
              (+ (play-map mine)
                 (outcome-map [theirs mine]))))
       (apply +)))

(defn part2
  [inputs]
  (->> inputs
       (map (fn [[theirs mine]]
              (aggregate-map [theirs mine])))
       (apply +)))

(comment
  (part1 (utils/load-edn-input "2022/day2.edn"))
  (part2 (utils/load-edn-input "2022/day2.edn")))
