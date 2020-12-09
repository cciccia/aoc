(ns cciccia.aoc.2020.day1
  (:require [clojure.math.combinatorics :as combo]
            [cciccia.aoc.utils :as utils]))

(defn part1
  [input]
  (reduce (fn [seen test]
            (if (contains? seen (- 2020 test))
              (reduced (* test (- 2020 test)))
              (conj seen test)))
          #{}
          input))

(defn part2-shitty
  [input]
  (some (fn [[a b c]]
          (when (= 2020 (+ a b c))
            (* a b c)))
        (combo/combinations input 3)))

(defn part2-better
  [input]
  (let [twos (reduce (fn [acc [a b]]
                       (assoc acc (+ a b) [a b]))
                     {}
                     (combo/combinations input 2))]
    (some (fn [test]
            (when (contains? twos (- 2020 test))
              (apply * (conj (get twos (- 2020 test)) test))))
          input)))


(comment
  (time (part1 (utils/load-edn-input "day1.edn"))))

(comment
  (time (part2-shitty (utils/load-edn-input "day1.edn"))))

(comment
  (time (part2-better (utils/load-edn-input "day1.edn"))))
