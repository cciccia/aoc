(ns cciccia.aoc.2021.day6
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [input rounds]
  (->> (reduce
         (fn [p _i]
           (let [new-p (reduce
                         (fn [p2 [current amount]]
                           (if (zero? current)
                             (-> (update p2
                                         8
                                         #(+ (or % 0) amount))
                                 (update 6
                                         #(+ (or % 0) amount))
                                 (update current
                                         #(- (or % 0) amount)))
                             (-> (update p2
                                         (dec current)
                                         #(+ (or % 0) amount))
                                 (update current
                                         #(- (or % 0) amount)))))
                         {}
                         p)]
             (merge-with + p new-p)))
         (frequencies input)
         (range rounds))
       vals
       (apply +)))

(comment
  (part1 (utils/load-edn-input "2021/day6.edn") 80)
  (part1 (utils/load-edn-input "2021/day6.edn") 256)
  (part1 (utils/load-edn-input "2021/day6-sample.edn") 18))
