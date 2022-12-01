(ns cciccia.aoc.y2020.day8
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [instructions]
  (loop [pos 0
         accumulator 0
         seen #{}]
    (let [[instruction value] (get instructions pos)]
      (cond
        (nil? instruction)
        [accumulator true]

        (contains? seen pos)
        [accumulator false]

        (= instruction "nop")
        (recur (inc pos) accumulator (conj seen pos))

        (= instruction "acc")
        (recur (inc pos) (+ accumulator value) (conj seen pos))

        (= instruction "jmp")
        (recur (+ pos value) accumulator (conj seen pos))
  
        :else
        (throw (ex-info "Oh god" {:instruction instruction :value value}))))))

(defn swaps
  [instructions]
  (reduce
    (fn [acc [instruction value i]]
      (case instruction
        "acc"
        acc

        "jmp"
        (conj acc (assoc instructions i ["nop" value]))

        "nop"
        (conj acc (assoc instructions i ["jmp" value]))))
    []
    (map-indexed (fn [i [instruction value]]
                   (vector instruction value i))
                 instructions)))

(defn part2
  [instructions]
  (some (fn [instruction-swap]
          (let [[accumulator done?] (part1 instruction-swap)]
            (when done?
              accumulator)))
        (swaps instructions)))

(comment
  (part1 (utils/load-edn-input "day8.edn")))

(comment
  (part2 (utils/load-edn-input "day8.edn")))
