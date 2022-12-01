(ns cciccia.aoc.y2020.day9
  (:require [cciccia.aoc.utils :as utils]))

(defn entry-valid-for-premable?
  [preamble-set test]
  (some (fn [preamble-entry]
          (and (not= preamble-entry (- test preamble-entry))
               (contains? preamble-set (- test preamble-entry))))
        preamble-set))

(defn part1
  [input]
  (loop [preamble (vec (take 25 input))
         stream   (drop 25 input)]
    (let [preamble-set (set preamble)
          test         (first stream)]
      (if (entry-valid-for-premable? preamble-set test)
        (recur (conj (subvec preamble 1) test) (rest stream))
        test))))

(defn sum-until-bust
  [sub-input target]
  (reduce
    (fn [[acc v] n]
      (let [sum (+ acc n)]
        (cond
          (= sum target)
          (let [sorted (sort v)]
            (reduced (+ (first sorted) (last sorted))))

          (> sum target)
          (reduced nil)

          (< sum target)
          [(+ acc n) (conj v n)])))
    [0 []]
    sub-input))

(defn part2
  [input target]
  (some
    (fn [i]
      (sum-until-bust (drop i input) target))
    (range)))

(comment
  (part1 (utils/load-edn-input "day9.edn")))

(comment
  (part2 input 41682220))
