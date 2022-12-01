(ns cciccia.aoc.y2020.day2
  (:require [cciccia.aoc.utils :as utils]))

(defn valid?
  [min max c test]
  (<= min (get (frequencies test) c 0) max))

(defn part1
  [input]
  (->> input
       (filter #(apply valid? %))
       (count)))

(defn valid2?
  [posa posb c test]
  (let [a (get test (dec posa))
        b (get test (dec posb))]
    (and (not= a b)
         (or (= b c)
             (= a c)))))

(defn part2
  [input]
  (->> input
       (filter #(apply valid2? %))
       (count)))


(comment
  (part1 (utils/load-edn-input "day2.edn")))

(comment
  (part2 (utils/load-edn-input "day2.edn")))
