(ns cciccia.aoc.2021.day11
  (:require [cciccia.aoc.utils :as utils]))

(defn input->map
  [input]
  (reduce
    (fn [p1 [y line]]
      (merge p1
             (reduce
               (fn [p2 [x n]]
                 (assoc p2 [x y] n))
               {}
               (map-indexed #(vector %1 %2) line))))
    {}
    (map-indexed #(vector %1 %2) input)))

(defn increment-if-not-zero
  [input-map [x y]]
  (utils/update-if-exists input-map [x y] #(if (zero? %) % (inc %))))

(defn flash
  [input-map [x y]]
  (-> input-map
      (assoc [x y] 0)
      (increment-if-not-zero [(dec x) (dec y)])
      (increment-if-not-zero [x (dec y)])
      (increment-if-not-zero [(inc x) (dec y)])
      (increment-if-not-zero [(dec x) y])
      (increment-if-not-zero [(inc x) y])
      (increment-if-not-zero [(dec x) (inc y)])
      (increment-if-not-zero [x (inc y)])
      (increment-if-not-zero [(inc x) (inc y)])))

(defn flash-all
  [input-map]
  (let [to-flash (->> input-map
                      (filter (fn [[_coord val]]
                                (> val 9)))
                      (into {}))]
    (->> to-flash
         (reduce
           (fn [p [coord _val]]
             (flash p coord))
           input-map))))

(defn part1
  [input]
  (let [input-map (input->map input)]
    (loop [steps 0
           flashes 0
           current-input-map input-map]
      (let [incremented-input-map (->> current-input-map
                                       (map (fn [[coord val]]
                                              [coord (inc val)]))
                                       (into {}))]
        (if (= steps 100)
           flashes
           (let [next-input-map (utils/converge flash-all incremented-input-map)]
             (recur (inc steps)
                    (+ flashes (->> (filter (fn [[_coord val]]
                                              (zero? val))
                                            next-input-map)
                                    count))
                    next-input-map)))))))

(defn part2
  [input]
  (let [input-map (input->map input)]
    (loop [steps 0
           current-input-map input-map]
      (let [incremented-input-map (->> current-input-map
                                       (map (fn [[coord val]]
                                              [coord (inc val)]))
                                       (into {}))]
        (let [next-input-map (utils/converge flash-all incremented-input-map)
              flashes (->> (filter (fn [[_coord val]]
                                     (zero? val))
                                   next-input-map)
                           count)]
          (if (= flashes 100)
            (inc steps)
            (recur (inc steps)
                   next-input-map)))))))

(comment
  (part1 (utils/load-edn-input "2021/day11.edn"))
  (part2 (utils/load-edn-input "2021/day11.edn"))
  (part2 (utils/load-edn-input "2021/day11-sample.edn")))
