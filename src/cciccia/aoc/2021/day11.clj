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

(defn step
  [[input-map flashes]]
  (->> input-map
       (map (fn [[coord val]]
              [coord (inc val)]))
       (into {})
       (utils/converge flash-all)
       ((juxt identity #(->> %
                             (filter (fn [[_coord val]]
                                       (zero? val)))
                             count
                             (+ flashes))))))

(defn part1
  [input]
  (second (first (drop 100 (iterate step [(input->map input) 0])))))

(defn part2
  [input]
  (->> (iterate step [(input->map input) 0])
       (map-indexed #(vector %1 %2))
       (some (fn [[i [grid _flashes]]]
               (when (nil? (some (fn [[_coord val]]
                                   (pos? val))
                                 grid))
                 i)))))

(comment
  (part1 (utils/load-edn-input "2021/day11.edn"))
  (part2 (utils/load-edn-input "2021/day11.edn"))
  (part2 (utils/load-edn-input "2021/day11-sample.edn")))
