(ns cciccia.aoc.y2022.day15
  (:require [cciccia.aoc.utils :as utils]))

(defn- manhattan-diamond
  "slow as molasses"
  [[sx sy] distance]
  (mapcat
    (fn [x-offset]
      (let [minor-distance (Math/abs (- x-offset sx))]
        (map
          (fn [y-offset]
            (println x-offset y-offset)
            [(+ sx x-offset) (+ sy y-offset)])
          (range (* -1 minor-distance) (inc minor-distance)))))
    (range (* -1 distance) (inc distance))))

(defn- manhattan-diamond-perimeter
  [grid [sx sy] distance]
  (reduce
    (fn [acc x-offset]
      (let [minor-distance (Math/abs (- x-offset sx))]
        (-> acc
            (conj [(+ sx x-offset) (- sy minor-distance)])
            (conj [(+ sx x-offset) (+ sy minor-distance)]))))
    grid
    (range (* -1 distance) (inc distance))))



;(defn part1
;  [input]
;  (let [grid (->> input
;                  (reduce
;                    (fn [acc [[sx sy] [bx by]]]
;                      (let [distance   (utils/manhattan [sx sy] [bx by])]
;                        (println distance)
;                        (manhattan-diamond-perimeter acc [sx sy] distance)))
;                    #{}))]
;    (->> grid
;         (filter (fn [[[_x y] v]]
;                   (and (= y row-y)
;                        (= v "X")))))))

(defn part1
  [input row-y beacon-x-in-row-y]
  (let [min-x (->> input
                   (map
                     (fn [[[sx sy] [bx by]]]
                       (- sx (utils/manhattan [sx sy] [bx by]))))
                   sort
                   first)
        max-x (->> input
                   (map
                     (fn [[[sx sy] [bx by]]]
                       (+ sx (utils/manhattan [sx sy] [bx by]))))
                   sort
                   last)]
    (loop [x   min-x
           cnt 0]
      (let [jump (apply max (map
                              (fn [[[sx sy] [bx by]]]
                                (let [our-manhattan (utils/manhattan [sx sy] [x row-y])
                                      ref-manhattan (utils/manhattan [sx sy] [bx by])
                                      x-adjust (if (< x sx) (* 2 (- sx x)) 0)]
                                  (if (<= our-manhattan ref-manhattan)
                                    (inc (+ x-adjust (- ref-manhattan our-manhattan)))
                                    -1)))
                              input))]
        (if (= jump -1)
          (if (> x max-x)
            cnt
            (recur (inc x) cnt))
          (recur (+ x jump) (if (< x beacon-x-in-row-y (+ x jump))
                              (dec (+ cnt jump))
                              (+ cnt jump))))))))

(defn part2
  [input limit]
  (let [beacons (->> input
                     (map
                       (fn [[[_sx _sy] [bx by]]]
                         [bx by]))
                     set)]
    (loop [[x y] [0 0]]
      (let [jump (apply max (map
                              (fn [[[sx sy] [bx by]]]
                                (let [our-manhattan (utils/manhattan [sx sy] [x y])
                                      ref-manhattan (utils/manhattan [sx sy] [bx by])
                                      x-adjust (if (< x sx) (* 2 (- sx x)) 0)]
                                  (if (<= our-manhattan ref-manhattan)
                                    (inc (+ x-adjust (- ref-manhattan our-manhattan)))
                                    (if (contains? beacons [x y]) 1 -1))))
                              input))]
        (if (= -1 jump)
          (do
            (println [x y])
            (+ (* x 4000000) y))
          (if (> (+ x jump) limit)
            (recur [0 (inc y)])
            (recur [(+ x jump) y])))))))

(comment
  (part1 (utils/load-edn-input "2022/day15.edn") 2000000 2733699)
  (part1 (utils/load-edn-input "2022/day15-sample.edn") 10 2)

  (part2 (utils/load-edn-input "2022/day15-sample.edn") 20)
  (part2 (utils/load-edn-input "2022/day15.edn") 4000000))
