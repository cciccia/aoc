(ns cciccia.aoc.2020.day10
  (:require [cciccia.aoc.utils :as utils]
            [taoensso.timbre :as timbre]
            [clojure.math.combinatorics :as combo]))

(defn part1
  [input]
  (let [joltages-sorted (sort input)
        target (+ 3 (apply max joltages-sorted))
        joltages-set (conj (set input) 0 target)
        {:keys [ones threes] :as res} (reduce
                                        (fn [acc joltage]
                                          (cond
                                            (contains? joltages-set (+ joltage 1))
                                            (update acc :ones conj [joltage (+ joltage 1)])

                                            (contains? joltages-set (+ joltage 2))
                                            (update acc :ones conj [joltage (+ joltage 2)])

                                            (contains? joltages-set (+ joltage 3))
                                            (update acc :threes conj [joltage (+ joltage 3)])

                                            (= joltage target)
                                            (reduced acc)

                                            :else
                                            (reduced nil)))
                                        {:ones []
                                         :twos []
                                         :threes []}
                                        (sort joltages-set))]
    (when res
      (* (count ones) (count threes)))))

(defn valid-joltage-count*
  [n]
  (->> (combo/subsets (range 1 n))
       (map #(conj % 0 n))
       (filter part1)
       (count)))

(def valid-joltage-count (memoize valid-joltage-count*))

(defn part2
  [input]
  (let [joltages-sorted (sort input)
        joltages-set (conj (set input) 0 (+ 3 (apply max joltages-sorted)))]
    (loop [joltages-sorted (sort joltages-set)
           contiguous-count 1
           matches 1]
      (let [current (first joltages-sorted)
            remaining (rest joltages-sorted)]
        (cond
          (not (seq remaining))
          matches

          (contains? joltages-set (inc current))
          (recur remaining (inc contiguous-count) matches)

          :else
          (recur remaining 1 (* matches (valid-joltage-count (dec contiguous-count)))))))))


(comment
  (part1 (utils/load-edn-input "day10.edn")))

(comment
  (time (part2 (utils/load-edn-input "day10.edn"))))