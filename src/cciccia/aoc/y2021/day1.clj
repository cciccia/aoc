(ns cciccia.aoc.y2021.day1
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [input]
  (:tot (reduce
          (fn [{:keys [last tot]} c]
            {:last c
             :tot (cond-> tot
                          (> c last)
                          inc)})
          {:last 99999 :tot 0}
          input)))

(defn part2
  [input]
  (:tot (reduce
          (fn [{:keys [last tot]} c]
            (let [new-last (cons c (drop-last last))]
              {:last new-last
               :tot (cond-> tot
                            (and (= 3 (count (remove nil? last)))
                                 (> (apply + new-last) (apply + last)))
                            inc)}))
          {:last (seq [nil nil nil]) :tot 0}
          input)))

(comment
  (part1 (utils/load-edn-input "2021/day1.edn"))
  (part2 (utils/load-edn-input "2021/day1.edn")))
