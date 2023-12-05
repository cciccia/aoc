(ns cciccia.aoc.y2023.day5
  (:require [cciccia.aoc.utils :as utils]))
(defn part1
  [maps seeds]
  (->> (map (fn [seed]
              (reduce
                (fn [acc cur-map]
                  (or (some (fn [[dest src range]]
                              (when (<= src acc (dec (+ src range)))
                                (+ dest (- acc src))))
                            cur-map)
                      acc))
                seed
                maps))
            seeds)
       (apply min)))

(defn part2
  [maps seeds]

  (->> (some (fn [i]
               (let [seed (reduce
                            (fn [acc cur-map]
                              (or (some (fn [[dest src range]]
                                          (when (<= dest acc (dec (+ dest range)))
                                            (+ src (- acc dest))))
                                        cur-map)
                                  acc))
                            i
                            (reverse maps))]
                 (some (fn [[s r]]
                         (when (<= s seed (dec (+ s r)))
                           i))
                       (partition 2 seeds))))
             (range))))


(comment
  (part1 (utils/load-edn-input "2023/day5-maps.edn") (utils/load-edn-input "2023/day5-seeds.edn"))
  (part2 (utils/load-edn-input "2023/day5-maps.edn") (utils/load-edn-input "2023/day5-seeds.edn")))

