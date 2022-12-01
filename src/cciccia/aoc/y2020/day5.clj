(ns cciccia.aoc.y2020.day5
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]))


(defn part1
  [input]
  (->> input
       (map (fn [[row col]]
              (+ (* (Integer/parseInt row 2) 8)
                 (* (Integer/parseInt col 2)))))
       (apply max)))

(defn part2
  [input]
  (->> input
       (reduce (fn [acc [row col]]
                 (conj acc (+ (* (Integer/parseInt row 2) 8)
                              (* (Integer/parseInt col 2)))))
               #{})
       (set/difference (set (range 947)))))

(comment
  (part1 (utils/load-edn-input "day5.edn")))

(comment
  (part2 (utils/load-edn-input "day5.edn")))
