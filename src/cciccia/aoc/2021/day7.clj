(ns cciccia.aoc.2021.day7
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [input]
  (Math/round ^double (/ (apply + input) (count input))))

(defn part1
  [input]
  (let [sorted-input (sort input)]
    (reduce (fn [p c]
              (let [new-val (apply + (map #(Math/abs (- c %)) sorted-input))]
                (if (< new-val p)
                  new-val
                  p)))
            9999999
            (range (first sorted-input) (inc (last sorted-input))))))

(def triangular (memoize (fn [n]
                           (apply + (range (inc n))))))

(defn part2
  [input]
  (let [sorted-input (sort input)]
    (reduce (fn [p c]
              (let [new-val (apply + (map #(triangular (Math/abs (- c %))) sorted-input))]
                (if (< new-val p)
                  new-val
                  p)))
            999999999
            (range (first sorted-input) (inc (last sorted-input))))))



(comment
  (part1 (utils/load-edn-input "2021/day7.edn"))
  (part2 (utils/load-edn-input "2021/day7.edn")))