(ns cciccia.aoc.y2020.day15
  (:require [taoensso.timbre :as timbre]))

(defn part1
  [input target]
  (let [starting (->> input
                      (map-indexed (fn [i n]
                                     [n (list (inc i))]))
                      (into {}))]
    (loop [db starting
           last-number (last input)
           turn (inc (count input))]
      (let [next-number (let [[t1 t2] (take 2 (get db last-number))]
                          (if t2
                            (- t1 t2)
                            0))]
        (if (= target turn)
          next-number
          (recur (update db next-number conj turn)
                 next-number
                 (inc turn)))))))

(comment
  (part1 [1 0 15 2 10 13] 2020))

(comment
  (part1 [1 0 15 2 10 13] 30000000))


