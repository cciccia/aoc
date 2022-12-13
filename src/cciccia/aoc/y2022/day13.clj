(ns cciccia.aoc.y2022.day13
  (:require [cciccia.aoc.utils :as utils]))

(defn compare-thing
  [l1 l2]
  (cond
    (and (nil? l1)
         (not (nil? l2)))
    -1

    (and (not (nil? l1))
         (nil? l2))
    1

    (and (not (sequential? l1))
         (not (sequential? l2))
         (< l1 l2))
    -1

    (and (not (sequential? l1))
         (not (sequential? l2))
         (> l1 l2))
    1

    (and (not (sequential? l1))
         (not (sequential? l2)))
    nil

    (and (sequential? l1)
         (sequential? l2))
    (some (fn [i] (compare-thing (get l1 i) (get l2 i))) (range (max (count l1) (count l2))))

    (and (not (sequential? l1))
         (sequential? l2))
    (compare-thing [l1] l2)

    (and (sequential? l1)
         (not (sequential? l2)))
    (compare-thing l1 [l2])))

(defn part1
  [input]
  (->> input
       (map-indexed (fn [i [l1 l2]]
                      [i (compare-thing l1 l2)]))
       (filter #(= -1 (second %)))
       (map #(inc (first %)))
       (apply +)))

(defn part2
  [input]
  (->> (apply concat input)
       (sort compare-thing)
       (map-indexed #(vector %1 %2))
       (keep (fn [[i v]]
               (when (or (= v [[2]])
                         (= v [[6]]))
                 (inc i))))
       (apply *)))

(comment
  (part1 (utils/load-edn-input "2022/day13.edn"))
  (part2 (utils/load-edn-input "2022/day13-two.edn")))
