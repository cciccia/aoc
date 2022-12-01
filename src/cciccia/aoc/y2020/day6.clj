(ns cciccia.aoc.y2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [taoensso.timbre :as timbre]))

(defn part1
  []
  (let [input (slurp (io/resource "day6.txt"))
        groups (str/split input #"\n\n")]
    (reduce (fn [acc2 group]
              (+ acc2 (->> (str/split group #"\n")
                           (reduce
                             (fn [acc line]
                               (set/union acc (set line)))
                             #{})
                           (count))))
            0
            groups)))

(comment
  (part1))

(defn part2
  []
  (let [input (slurp (io/resource "day6.txt"))
        groups (str/split input #"\n\n")]
    (reduce (fn [acc2 group]
              (+ acc2 (->> (str/split group #"\n")
                           (reduce
                             (fn [acc line]
                               (set/intersection acc (set line)))
                             (set (map char (range 97 123))))
                           (count))))
            0
            groups)))


(comment
  (part2))
