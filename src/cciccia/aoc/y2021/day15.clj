(ns cciccia.aoc.y2021.day15
  (:require [cciccia.aoc.utils :as utils]))

(defn get-neighbors
  [grid-map [x y] total-risk total-risk-map]
  (->> [[(dec x) y]
        [(inc x) y]
        [x (dec y)]
        [x (inc y)]]
       (keep
         (fn [[x2 y2]]
           (when
             (and (contains? grid-map [x2 y2])
                  (or (not (contains? total-risk-map [x2 y2]))
                      (> (get total-risk-map [x2 y2]) (+ total-risk (get grid-map [x2 y2])))))
             [[x2 y2] (+ total-risk (get grid-map [x2 y2]))])))))

(defn get-neighbors-no-backsies
  [grid-map [x y] total-risk total-risk-map]
  (->> [[(inc x) y]
        [x (inc y)]]
       (keep
         (fn [[x2 y2]]
           (when
             (and (contains? grid-map [x2 y2])
                  (or (not (contains? total-risk-map [x2 y2]))
                      (> (get total-risk-map [x2 y2]) (+ total-risk (get grid-map [x2 y2])))))
             [[x2 y2] (+ total-risk (get grid-map [x2 y2]))])))))

(defn remove-dead-paths
  [queue new-neighbors]
  (reduce
    (fn [p [neighbor-position neighbor-total-risk]]
      (if-let [[k v] (some (fn [[i [position total-risk]]]
                             (when (= position neighbor-position)
                               [i [position (min total-risk neighbor-total-risk)]]))
                           (map-indexed #(vector %1 %2) p))]
        (assoc p k v)
        (conj p [neighbor-position neighbor-total-risk])))
    queue
    new-neighbors))

(defn expand-grid
  [grid-map]
  (let [max-x (apply max (map first (keys grid-map)))
        max-y (apply max (map second (keys grid-map)))]
    (->> (map
           (fn [i]
             (->> grid-map
                  (map
                    (fn [[[x y] v]]
                      (let [new-x (+ x (* (inc max-x) (mod i 5)))
                            new-y (+ y (* (inc max-y) (long (Math/floor (/ i 5)))))
                            new-val (inc (mod (dec (+ v (+ (mod i 5) (long (Math/floor (/ i 5)))))) 9))]
                        [[new-x new-y] new-val])))
                  (into {})))
           (range 25))
         (apply merge))))

(defn part1
  [grid-map]
  (let [max-x (apply max (map first (keys grid-map)))
        max-y (apply max (map second (keys grid-map)))]
    (loop [queue [[[0 0] 0]]
           total-risk-map {}]
      (let [[position total-risk] (first queue)]
        (if (nil? position)
          (get total-risk-map [max-x max-y])
          (let [new-total-risk-map (assoc total-risk-map position total-risk)
                new-queue (remove-dead-paths queue (get-neighbors grid-map position total-risk new-total-risk-map))]
            (recur (subvec new-queue 1) new-total-risk-map)))))))

(defn part2
  [grid-map]
  (let [max-x (apply max (map first (keys grid-map)))
        max-y (apply max (map second (keys grid-map)))]
    (loop [queue [[[0 0] 0]]
           total-risk-map {}]
      (let [[position total-risk] (first queue)]
        (if (contains? total-risk-map [max-x max-y])
          (get total-risk-map [max-x max-y])
          (let [new-total-risk-map (assoc total-risk-map position total-risk)
                new-queue (->> (get-neighbors grid-map position total-risk new-total-risk-map)
                               (remove-dead-paths queue)
                               (drop 1)
                               (sort-by second)
                               vec)]
            (recur new-queue new-total-risk-map)))))))

(comment
  (part1 (utils/two-d-grid->map (utils/load-edn-input "2021/day15.edn")))
  (part1 (utils/two-d-grid->map (utils/load-edn-input "2021/day15-sample.edn")))
  (part1 (utils/two-d-grid->map (utils/load-edn-input "2021/day15-sample-2.edn")))

  (part2 (expand-grid (utils/two-d-grid->map (utils/load-edn-input "2021/day15-sample.edn"))))
  (part2 (expand-grid (utils/two-d-grid->map (utils/load-edn-input "2021/day15.edn")))))
