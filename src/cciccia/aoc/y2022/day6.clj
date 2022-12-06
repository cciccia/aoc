(ns cciccia.aoc.y2022.day6
  (:require [clojure.java.io :as io]))

(defn part1
  [input len]
  (some
    (fn [[i group]]
      (when (apply distinct? group)
        (+ i len)))
    (map-indexed #(vector %1 %2) (partition len 1 input))))

(comment
  (part1 (slurp (io/resource "2022/day6.txt")) 4)
  (part1 (slurp (io/resource "2022/day6.txt")) 14))

