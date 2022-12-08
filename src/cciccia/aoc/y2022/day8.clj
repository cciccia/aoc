(ns cciccia.aoc.y2022.day8
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]
            [com.climate.claypoole :as cp]))

(defn- all-visible
  [grid vertical? reverse? minor-axis-val]
  (let [max-major-axis-val (->> grid keys (map (if vertical? second first)) (apply max))]
    (reduce
      (fn [acc major-axis-val]
        (let [coord      (if vertical?
                           [minor-axis-val major-axis-val]
                           [major-axis-val minor-axis-val])
              new-height (get grid coord)]
          (if (= (if reverse? 0 max-major-axis-val) major-axis-val)
            (cond-> (:visible acc)
                    (> new-height (:height acc))
                    (conj coord))
            (cond-> acc
                    (> new-height (:height acc))
                    (-> (assoc :height new-height) (update :visible conj coord))))))
      {:height  -1
       :visible #{}}
      (cond-> (range (inc max-major-axis-val))
              reverse?
              reverse))))

(defn part1
  [grid]
  (let [max-x (->> grid keys (map first) (apply max))
        max-y (->> grid keys (map second) (apply max))
        trees (set/union
                (apply set/union (map (partial all-visible grid false false) (range (inc max-y))))
                (apply set/union (map (partial all-visible grid false true) (range (inc max-y))))
                (apply set/union (map (partial all-visible grid true false) (range (inc max-x))))
                (apply set/union (map (partial all-visible grid true true) (range (inc max-x)))))]
    (count trees)))

(defn- score
  [grid coord vertical? reverse? max-major-axis-val]
  (let [height             (get grid coord)
        minor-axis-val     (if vertical? (first coord) (second coord))]
    (reduce
      (fn [acc major-axis-val]
        (let [new-coord (if vertical?
                          [minor-axis-val major-axis-val]
                          [major-axis-val minor-axis-val])]
          (if (>= (get grid new-coord)
                  height)
            (reduced (inc acc))
            (inc acc))))
      0
      (range ((if reverse? dec inc) (if vertical? (second coord) (first coord)))
             (if reverse? -1 (inc max-major-axis-val))
             (if reverse? -1 1)))))

(defn part2
  [grid]
  (let [max-x (->> grid keys (map first) (apply max))
        max-y (->> grid keys (map second) (apply max))]
    (->> (cp/pfor 32 [x (range (inc max-x))
                      y (range (inc max-y))]
           (*
             (score grid [x y] false false max-x)
             (score grid [x y] false true max-x)
             (score grid [x y] true false max-y)
             (score grid [x y] true true max-y)))
         sort
         reverse
         first)))

(comment
  (part1 (-> (utils/lined-spaced-input->2d-vec-int "2022/day8.txt" #"")
             (utils/two-d-grid->map)))
  (part2 (-> (utils/lined-spaced-input->2d-vec-int "2022/day8.txt" #"")
             (utils/two-d-grid->map))))
