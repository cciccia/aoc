(ns cciccia.aoc.y2023.day2
  (:require [cciccia.aoc.utils :as utils]
            [clojure.string :as str]))

(defn games
  [file]
  (->> (utils/lined-spaced-input->str file)
       (reduce (fn [acc line]
                 (let [[_ game trials] (re-matches #"Game (\d+): (.*)" line)]
                   (assoc acc (Integer/parseInt game)
                              (reduce (fn [acc2 trial]
                                        (merge-with max
                                                    acc2
                                                    (reduce
                                                      (fn [acc3 pick]
                                                        (let [[_ num color] (re-matches #"(\d+) (\w+)" pick)]
                                                          (assoc acc3 (keyword color) (Integer/parseInt num))))
                                                      {}
                                                      (str/split trial #", "))))
                                      {:blue 0
                                       :red 0
                                       :green 0}
                                      (str/split trials #"; ")))))
               {})))

(defn part1
  [file]
  (->> (games file)
       (keep (fn [[id {:keys [red blue green]}]]
               (when (and (<= red 12)
                          (<= green 13)
                          (<= blue 14))
                 id)))
       (apply +)))

(defn part2
  [file]
  (->> (games file)
       (map (fn [[_id {:keys [red blue green]}]]
              (* red blue green)))
       (apply +)))

(comment
  (part1 "2023/day2.txt")
  (part2 "2023/day2.txt"))