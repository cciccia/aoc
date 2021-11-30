(ns cciccia.aoc.day1
  (:require [cciccia.aoc.utils :as utils]))

(defn fuel
  [m]
  (- (quot m 3) 2))

(declare fuel-with-remainder)

(defn fuel-with-remainder*
  [m]
  (let [base (fuel m)]
    (if (pos? base)
      (+ base (fuel-with-remainder base))
      (max base 0))))

(def fuel-with-remainder (memoize fuel-with-remainder*))

(defn part2
  [masses]
  (->> masses
       (map fuel-with-remainder)
       (apply +)))

(defn part1
  [masses]
  (->> masses
       (map fuel)
       (apply +)))

(comment
  (part1 (utils/load-edn-input "2019/day1.edn"))
  (part2 (utils/load-edn-input "2019/day1.edn")))