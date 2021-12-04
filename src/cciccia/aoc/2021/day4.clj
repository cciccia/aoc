(ns cciccia.aoc.2021.day4
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]))

(defn winner-winner-chicken-dinner?
  [grid called-set]
  (some
    (fn [row-or-col]
      (set/subset? (set row-or-col) called-set))
    (concat grid (utils/transpose-2d-vector grid))))

(defn part1
  [grids all-called]
  (loop [to-be-called (seq all-called)
         called       []]
    (if-let [winning-grid (some
                            (fn [grid]
                              (when (winner-winner-chicken-dinner? grid (set called))
                                grid))
                            grids)]
      (* (last called) (apply + (set/difference (set (flatten winning-grid)) (set called))))
      (recur (drop 1 to-be-called)
             (conj called (first to-be-called))))))

(defn part2
  [grids all-called]
  (loop [to-be-called (seq all-called)
         called       []
         winners      #{}]
    (let [non-winners (set/difference (set grids) winners)
          new-winners (filter (fn [grid]
                                (winner-winner-chicken-dinner? grid (set called)))
                              non-winners)]
      (if (and (= 99 (count winners))
               (= 1 (count new-winners)))
        (* (last called) (apply + (set/difference (set (flatten (first new-winners)))
                                                  (set called))))
        (recur (drop 1 to-be-called)
               (conj called (first to-be-called))
               (apply conj winners new-winners))))))

(comment
  (part1 (utils/load-edn-input "2021/day4-grids.edn") (utils/load-edn-input "2021/day4-calls.edn"))
  (part2 (utils/load-edn-input "2021/day4-grids.edn") (utils/load-edn-input "2021/day4-calls.edn")))


