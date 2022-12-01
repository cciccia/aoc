(ns cciccia.aoc.y2021.day9
  (:require [cciccia.aoc.utils :as utils]))

(defn lowest?
  [input-map [x y]]
  (and (< (get input-map [x y]) (get input-map [(inc x) y] 10))
       (< (get input-map [x y]) (get input-map [(dec x) y] 10))
       (< (get input-map [x y]) (get input-map [x (inc y)] 10))
       (< (get input-map [x y]) (get input-map [x (dec y)] 10))))

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

(defn part1
  [input]
  (let [input-map (input->map input)]
    (->> input-map
         (keep (fn [[coord val]]
                 (when (lowest? input-map coord)
                   val)))
         (map inc)
         (apply +))))

(defn find-next
  [input-map [x y]]
  (cond
    (> (get input-map [x y]) 8)
    nil

    (> (get input-map [x y]) (get input-map [(inc x) y] 10))
    [(inc x) y]

    (> (get input-map [x y]) (get input-map [(dec x) y] 10))
    [(dec x) y]

    (> (get input-map [x y]) (get input-map [x (inc y)] 10))
    [x (inc y)]

    (> (get input-map [x y]) (get input-map [x (dec y)] 10))
    [x (dec y)]

    :else
    nil))

(defn basin2
  [input-map coord]
  (loop [loc coord]
    (let [val (get input-map loc)
          next-loc (find-next input-map loc)]
      (if (nil? next-loc)
        (if (= 9 val)
          nil
          loc)
        (recur next-loc)))))

(defn part2
  [input]
  (let [input-map (input->map input)]
    (->> input-map
         (map (fn [[coord]]
                (basin2 input-map coord)))
         frequencies
         (remove (fn [[coord]]
                   (nil? coord)))
         (map second)
         sort
         reverse
         (take 3)
         (apply *))))

(comment
  (part1 (utils/load-edn-input "2021/day9.edn"))
  (part2 (utils/load-edn-input "2021/day9.edn"))
  (part2 (utils/load-edn-input "2021/day9-sample.edn")))
