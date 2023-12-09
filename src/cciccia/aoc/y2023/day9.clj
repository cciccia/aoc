(ns cciccia.aoc.y2023.day9
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [inputs backwards?]
  (->> inputs
       (map
         (fn [input]
           (reduce
             (fn [acc _]
               (let [new (mapv
                           (fn [i]
                             (- (get (last acc) (inc i)) (get (last acc) i)))
                           (range (dec (count (last acc)))))]
                 (if (every? zero? new)
                   (reduced (conj acc new))
                   (conj acc new))))
             [input]
             (range))))
       (map reverse)
       (map (fn [histories]
              (reduce
                (fn [acc history]
                  (if backwards?
                    (- (first history) acc)
                    (+ (last history) acc)))
                0
                histories)))
       (apply +)))

(comment
  (part1 (utils/load-edn-input "2023/day9.edn") true))


