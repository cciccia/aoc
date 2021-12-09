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
  "slowly build a map of numbers to patterns such that we can quickly compare sets of letters
  by cross referencing the semantic meanings that their keys (numbers) represent
  then we invert that relationship to map sequences to numbers for the output later"
  (->> (reduce
         (fn [key i]
           (let [pattern (set (map str (get patterns (mod i (count patterns)))))]
             (cond
               ; done!
               (= 10 (count (keys key)))
               (reduced key)

               ;these ones all have a distinct count
               (= 2 (count pattern))
               (assoc key 1 pattern)

               (= 3 (count pattern))
               (assoc key 7 pattern)

               (= 4 (count pattern))
               (assoc key 4 pattern)

               (= 7 (count pattern))
               (assoc key 8 pattern)

               ; next three are six digits (9, 6, 0)
               ; 9 is the only numbr comprised of both 7 and 4
               (and (= 6 (count pattern))
                    (some? (get key 7))
                    (some? (get key 4))
                    (set/subset? (get key 7) pattern)
                    (set/subset? (get key 4) pattern))
               (assoc key 9 pattern)

               ; if we've done 9 and 1 is in it, it's definitely 0, not 6
               (and (= 6 (count pattern))
                    (some? (get key 9))
                    (some? (get key 1))
                    (set/subset? (get key 1) pattern))
               (assoc key 0 pattern)

               ; otherwise, it's 6
               (and (= 6 (count pattern))
                    (some? (get key 9))
                    (some? (get key 1))
                    (not (set/subset? (get key 1) pattern)))
               (assoc key 6 pattern)

               ; 5 digits (2, 3, 5)
               ; only 3 has 1 in it
               (and (= 5 (count pattern))
                    (some? (get key 1))
                    (set/subset? (get key 1) pattern))
               (assoc key 3 pattern)

               ; 6 has 5 on it
               (and (= 5 (count pattern))
                    (some? (get key 6))
                    (set/subset? pattern (get key 6)))
               (assoc key 5 pattern)

               ; someone has to get picked last
               (= 9 (count (keys key)))
               (assoc key 2 pattern)

               ; we don't have enough information to figure out what this is
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