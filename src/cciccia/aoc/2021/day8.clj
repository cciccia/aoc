(ns cciccia.aoc.2021.day8
  (:require [cciccia.aoc.utils :as utils]
            [clojure.set :as set]))

(defn part1
  [input]
  (->> input
       (map (fn [[_patterns output]]
              (count (filter (fn [number]
                               (contains? #{2 3 4 7} (count number)))
                             output))))
       (apply +)))

(defn build-key
  [patterns]
  (->> (reduce
         (fn [key i]
           (let [pattern (set (map str (get patterns (mod i (count patterns)))))]
             (cond
               (= 10 (count (keys key)))
               (reduced key)

               (= 2 (count pattern))
               (assoc key 1 pattern)

               (= 3 (count pattern))
               (assoc key 7 pattern)

               (= 4 (count pattern))
               (assoc key 4 pattern)

               (= 7 (count pattern))
               (assoc key 8 pattern)

               (and (some? (get key 7))
                    (some? (get key 4))
                    (set/subset? (get key 7) pattern)
                    (set/subset? (get key 4) pattern))
               (assoc key 9 pattern)

               (and (= 6 (count pattern))
                    (some? (get key 9))
                    (some? (get key 1))
                    (set/subset? (get key 1) pattern))
               (assoc key 0 pattern)

               (and (= 6 (count pattern))
                    (some? (get key 9))
                    (some? (get key 1))
                    (not (set/subset? (get key 1) pattern)))
               (assoc key 6 pattern)

               (and (= 5 (count pattern))
                    (some? (get key 1))
                    (set/subset? (get key 1) pattern))
               (assoc key 3 pattern)

               (and (= 5 (count pattern))
                    (some? (get key 6))
                    (set/subset? pattern (get key 6)))
               (assoc key 5 pattern)

               (= 9 (count (keys key)))
               (assoc key 2 pattern)

               :else
               key)))
         {}
         (iterate inc 0))
       (set/map-invert)))

(defn part2
  [input]
  (->> input
       (map (fn [[patterns output]]
              (let [key (build-key patterns)]
                (->> output
                     (map-indexed (fn [i word]
                                    (* (Math/pow 10 (- 3 i)) (get key (set (map str word))))))
                     (apply +)))))
       (apply +)))


(comment
  (part1 (utils/load-edn-input "2021/day8.edn"))
  (part2 (utils/load-edn-input "2021/day8.edn")))