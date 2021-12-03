(ns cciccia.aoc.2021.day3
  (:require [cciccia.aoc.utils :as utils]))

(defn g-or-e
  [input x ones?]
  (reduce
    (fn [p [i places]]
      (let [s (apply + places)]
        (cond
          (and ones?
               (>= s (/ (count input) 2)))
          (int (+ p (Math/pow 2 (- x i 1))))

          (and (not ones?)
               (< s (/ (count input) 2)))
          (int (+ p (Math/pow 2 (- x i 1))))

          :else
          p)))
    0
    (map-indexed #(vector %1 %2) (apply mapv vector input))))

(defn part1
  [input x]
  (let [gamma (g-or-e input x true)
        epsilon (g-or-e input x false)]
    (* gamma epsilon)))

(defn to-dec
  [val x]
  (reduce (fn [p [i v]] (+ p (int (* v (Math/pow 2 (- x i 1))))))
          0
          (map-indexed #(vector %1 %2) val)))

(defn attune
  [input target x i]
  (filter
    (fn [c]
      (zero? (bit-xor (bit-and target (int (Math/pow 2 (- x i 1))))
                      (bit-and (to-dec c x) (int (Math/pow 2 (- x i 1)))))))
    input))

(defn part2
  [input x]
  (let [o2 (loop [i 0
                  left input]
             (if (= 1 (count left))
               (to-dec (first left) x)
               (let [gamma (g-or-e left x true)]
                 (recur (inc i) (attune left gamma x i)))))
        co2 (loop [i 0
                   left input]
              (if (= 1 (count left))
                (to-dec (first left) x)
                (let [epsilon (g-or-e left x false)]
                  (recur (inc i) (attune left epsilon x i)))))]
    (* o2 co2)))


(comment
  (part1 (utils/load-edn-input "2021/day3.edn") 12)
  (part2 (utils/load-edn-input "2021/day3.edn") 12)
  (part2 (utils/load-edn-input "2021/day3.edn") 12)
  (part1 (utils/load-edn-input "2021/day3-sample.edn") 5)
  (part2 (utils/load-edn-input "2021/day3-sample.edn") 5))



