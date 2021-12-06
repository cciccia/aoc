(ns cciccia.aoc.2021.day5
  (:require [cciccia.aoc.utils :as utils]))

(defn plot-all-points
  [input diagonals?]
  (reduce
    (fn [p [[x1 y1] [x2 y2]]]
      (let [sx (cond (> x2 x1) 1 (< x2 x1) -1 :else 0)
            sy (cond (> y2 y1) 1 (< y2 y1) -1 :else 0)
            points (if (or diagonals? (zero? sx) (zero? sy))
                     (for [i (range (inc (max (Math/abs ^long (- x1 x2))
                                              (Math/abs ^long (- y1 y2)))))]
                       [(+ x1 (* sx i)) (+ y1 (* sy i))])
                     [])]
        (reduce
          (fn [q c]
            (update q c #(inc (or % 0))))
          p
          points)))
    {}
    input))

(defn part1
  [input diagonals?]
  (let [graph (plot-all-points input diagonals?)]
    (count (filter (fn [[_coord val]]
                     (> val 1))
                   graph))))

(comment
  (part1 (utils/load-edn-input "2021/day5.edn") false)
  (part1 (utils/load-edn-input "2021/day5.edn") true))
