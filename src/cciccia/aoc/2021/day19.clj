(ns cciccia.aoc.2021.day19
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]
            [clojure.math.combinatorics :as comb]))

(defn turn-x
  [[x y z]]
  [x (* -1 z) y])

(defn turn-y
  [[x y z]]
  [(* -1 z) y x])

(defn turn-z
  [[x y z]]
  [(* -1 y) x z])

(def all-orientations [identity
                       turn-x
                       (comp turn-x turn-x)
                       (comp turn-x turn-x turn-x)
                       turn-y
                       (comp turn-y turn-x)
                       (comp turn-y turn-x turn-x)
                       (comp turn-y turn-x turn-x turn-x)
                       (comp turn-y turn-y)
                       (comp turn-y turn-y turn-x)
                       (comp turn-y turn-y turn-x turn-x)
                       (comp turn-y turn-y turn-x turn-x turn-x)
                       (comp turn-y turn-y turn-y)
                       (comp turn-y turn-y turn-y turn-x)
                       (comp turn-y turn-y turn-y turn-x turn-x)
                       (comp turn-y turn-y turn-y turn-x turn-x turn-x)
                       turn-z
                       (comp turn-z turn-x)
                       (comp turn-z turn-x turn-x)
                       (comp turn-z turn-x turn-x turn-x)
                       (comp turn-z turn-z turn-z)
                       (comp turn-z turn-z turn-z turn-x)
                       (comp turn-z turn-z turn-z turn-x turn-x)
                       (comp turn-z turn-z turn-z turn-x turn-x turn-x)])

(defn offset
  [[x1 y1 z1] [x2 y2 z2]]
  [(- x1 x2) (- y1 y2) (- z1 z2)])

(defn adjust
  [[xd yd zd] [x y z]]
  [(+ x xd) (+ y yd) (+ z zd)])

(defn align
  [sensor-1 sensor-2]
  (some
    (fn [s1-coord]
      (some
        (fn [s2-coord]
          (let [s1-s2-offset (offset s1-coord s2-coord)
                s2-adjusted (set (map (partial adjust s1-s2-offset) sensor-2))]
            (when (>= (count (set/intersection sensor-1 s2-adjusted)) 12)
              {:adjustment s1-s2-offset
               :sensor s2-adjusted})))
        sensor-2))
    sensor-1))

(defn attempt-fit
  [sensor1 sensor2]
  (some
    (fn [orientation]
      (align (set sensor1) (set (map orientation sensor2))))
    all-orientations))

(defn fit-all-better
  [sensors]
  (loop [universe         (set (get sensors 0))
         sensor-locations {0 [0 0 0]}
         i                1]
    (let [i (mod i (count sensors))]
      (if (= (count (vals sensor-locations)) (count sensors))
        [universe sensor-locations]
        (if (contains? sensor-locations i)
          (recur universe sensor-locations (inc i))
          (let [sensor2 (get sensors i)]
            (if-let [{:keys [adjustment sensor]} (attempt-fit universe sensor2)]
              (do
                (println (inc (count sensor-locations)))
                (recur (set/union universe (set sensor)) (assoc sensor-locations i adjustment) (inc i)))
              (recur universe sensor-locations (inc i)))))))))

(defn part1
  [sensors]
  (count (first (fit-all-better sensors))))

(defn part2
  [sensors]
  (->> (comb/combinations (vals (second (fit-all-better sensors))) 2)
       (map (fn [[[x1 y1 z1] [x2 y2 z2]]]
              (+ (long (Math/abs ^long (- x1 x2)))
                 (long (Math/abs ^long (- y1 y2)))
                 (long (Math/abs ^long (- z1 z2))))))
       (apply max)))

(comment
  (part1 (utils/load-edn-input "2021/day19-sample.edn"))
  (part1 (utils/load-edn-input "2021/day19.edn"))
  (part2 (utils/load-edn-input "2021/day19-sample.edn"))
  (part2 (utils/load-edn-input "2021/day19.edn"))
  (map count (utils/load-edn-input "2021/day19-sample.edn")))