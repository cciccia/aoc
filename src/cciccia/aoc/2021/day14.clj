(ns cciccia.aoc.2021.day14
  (:require [cciccia.aoc.utils :as utils]
            [clojure.string :as str]))

(defn part1
  [start rules n]
  (let [amounts (->> (reduce
                       (fn [p _i]
                         (str/replace p #"(\w)(?=(\w))" #(str (% 1) (get rules (str (% 1) (% 2)) (% 2)))))
                       start
                       (range n))
                     frequencies
                     vals
                     sort)]
    (- (last amounts) (first amounts))))

(defn part2-not-shitty
  [start rules n]
  (let [starting-counts (reduce
                          (fn [p c]
                            (update p (apply str c) #(inc (or % 0))))
                          {}
                          (partition 2 1 start))
        amounts (->> (reduce
                       (fn [p _i]
                         (reduce
                           (fn [p2 [[s1 s2] v]]
                             (let [current (get p (str s1 s2) 0)]
                               (-> p2
                                   (update (str s1 v) #(+ current (or % 0)))
                                   (update (str v s2) #(+ current (or % 0))))))
                           {}
                           rules))
                       starting-counts
                       (range n))
                     (reduce
                       (fn [p [[s1 s2] v]]
                         (-> p
                             (update s1 #(+ v (or % 0)))
                             (update s2 #(+ v (or % 0)))))
                       {})
                     vals
                     (map #(long (Math/ceil (/ % 2))))
                     sort)]
    (- (last amounts) (first amounts))))

(comment
  (part1 "PKHOVVOSCNVHHCVVCBOH" (utils/load-edn-input "2021/day14.edn") 10)
  (part1 "NNCB" (utils/load-edn-input "2021/day14-sample.edn") 10)

  (time (part2-not-shitty "PKHOVVOSCNVHHCVVCBOH" (utils/load-edn-input "2021/day14.edn") 40))

  (part2-not-shitty "NNCB" (utils/load-edn-input "2021/day14-sample.edn") 10)
  (part2-not-shitty "NNCB" (utils/load-edn-input "2021/day14-sample.edn") 40))

