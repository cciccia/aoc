(ns cciccia.aoc.2021.day5.edn
  (:require [cciccia.aoc.utils :as utils]))

(defn plot-all-points
  [input]
  (reduce
    (fn [p [[x1 y1] [x2 y2]]]
      (if (or (= x1 x2) (= y1 y2))
        (let [points (for [x (if (> x2 x1)
                               (range x1 (inc x2) 1)
                               (range x2 (inc x1) 1))
                           y (if (> y2 y1)
                               (range y1 (inc y2) 1)
                               (range y2 (inc y1) 1))]
                       [x y])]
          (reduce
            (fn [q c]
              (update q c #(inc (or % 0))))
            p
            points))
        p))
    {}
    input))

(defn part1
  [input]
  (let [graph (plot-all-points input)]
    (count (filter (fn [[_coord val]]
                     (> val 1))
                   graph))))

(defn plot-all-points-2
  [input]
  (reduce
    (fn [p [[x1 y1] [x2 y2]]]
      (let [sx (cond (> x2 x1) 1 (< x2 x1) -1 :else 0)
            sy (cond (> y2 y1) 1 (< y2 y1) -1 :else 0)
            points (for [i (range (inc (max (Math/abs ^long (- x1 x2))
                                            (Math/abs ^long (- y1 y2)))))]
                     [(+ x1 (* sx i)) (+ y1 (* sy i))])]
        (reduce
          (fn [q c]
            (update q c #(inc (or % 0))))
          p
          points)))
    {}
    input))

(defn part2
  [input]
  (let [graph (plot-all-points-2 input)]
    (count (filter (fn [[_coord val]]
                     (> val 1))
                   graph))))

(comment
  (part1 (utils/load-edn-input "2021/day5.edn"))
  (part2 (utils/load-edn-input "2021/day5.edn")))

