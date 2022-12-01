(ns cciccia.aoc.y2020.day22
  (:require [cciccia.aoc.utils :as utils])
  (:import (clojure.lang PersistentQueue)))

(defn win
  [deck]
  (->> (reverse deck)
       (map-indexed
         (fn [i v]
           (* (inc i) v)))
       (apply +)))

(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))

(defn play
  [recursive-mode? [deck1 deck2]]
  (loop [d1 (queue deck1)
         d2 (queue deck2)
         seen #{}]
    (let [top1 (peek d1)
          top2 (peek d2)
          rest1 (pop d1)
          rest2 (pop d2)]
      (cond
        (contains? seen [(seq d1) (seq d2)])
        [true d1]

        (nil? top1)
        [false d2]

        (nil? top2)
        [true d1]

        (and recursive-mode?
             (>= (count (seq rest1)) top1)
             (>= (count (seq rest2)) top2))
        (let [p1-winner? (first (play true
                                      [(take top1 (seq rest1))
                                       (take top2 (seq rest2))]))]
          (if p1-winner?
            (recur (conj rest1 top1 top2)
                   rest2
                   (conj seen [(seq d1) (seq d2)]))
            (recur rest1
                   (conj rest2 top2 top1)
                   (conj seen [(seq d1) (seq d2)]))))
        (> top1 top2)
        (recur (conj rest1 top1 top2)
               rest2
               (conj seen [(seq d1) (seq d2)]))

        (< top1 top2)
        (recur rest1
               (conj rest2 top2 top1)
               (conj seen [(seq d1) (seq d2)]))))))

(defn part1
  [[deck1 deck2]]
  (win (second (play false [deck1 deck2]))))

(defn part2
  [[deck1 deck2]]
  (win (second (play true [deck1 deck2]))))


(comment
  (part1 (utils/load-edn-input "day22.edn")))

(comment
  (part2 (utils/load-edn-input "day22.edn")))

(comment
  (part2 (utils/load-edn-input "day22-sample.edn")))



