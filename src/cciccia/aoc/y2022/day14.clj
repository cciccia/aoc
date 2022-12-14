(ns cciccia.aoc.y2022.day14
  (:require [cciccia.aoc.utils :as utils]))

(defn move-sand
  [grid max-y coord]
  (if (= :done coord)
    :done
    (let [[x y] coord]
      (cond
        (> y max-y)
        :done

        (not (contains? grid [x (inc y)]))
        [x (inc y)]

        (not (contains? grid [(dec x) (inc y)]))
        [(dec x) (inc y)]

        (not (contains? grid [(inc x) (inc y)]))
        [(inc x) (inc y)]

        :else
        [x y]))))

(defn move-sand-2
  "this is off by 1 and i don't care"
  [grid max-y coord]
  (if (= :done coord)
    :done
    (let [[x y] coord]
      (cond
        (= (inc max-y) y)
        [x y]

        (not (contains? grid [x (inc y)]))
        [x (inc y)]

        (not (contains? grid [(dec x) (inc y)]))
        [(dec x) (inc y)]

        (not (contains? grid [(inc x) (inc y)]))
        [(inc x) (inc y)]

        (= [500 0] coord)
        :done

        :else
        [x y]))))


(defn gen-sand
  [part2? max-y grid]
  (let [new-sand (utils/converge (partial (if part2? move-sand-2 move-sand) grid max-y) [500 0])]
    (if (= :done new-sand)
      grid
      (assoc grid new-sand "o"))))

(defn part1
  [input part2?]
  (let [grid  (->> input
                   (reduce
                     (fn [acc line]
                       (apply assoc acc (->> (range (dec (count line)))
                                             (mapcat (fn [i]
                                                       (let [[start-x start-y] (get line i)
                                                             [end-x end-y] (get line (inc i))]
                                                         (if (= start-x end-x)
                                                           (map #(vector start-x %) (if (> end-y start-y)
                                                                                      (range start-y (inc end-y))
                                                                                      (range end-y (inc start-y))))
                                                           (map #(vector % start-y) (if (> end-x start-x)
                                                                                      (range start-x (inc end-x))
                                                                                      (range end-x (inc start-x))))))))
                                             (mapcat #(vector % "#")))))
                     {}))
        max-y (->> grid keys (map second) sort last)
        grid  (utils/converge (partial gen-sand part2? max-y) grid)]
    (cond-> (->> grid
                 (filter (fn [[_k v]]
                           (= v "o")))
                 count)
            part2? inc)))

(comment
  (part1 (utils/load-edn-input "2022/day14.edn") true))
