(ns cciccia.aoc.y2020.day17
  (:require [cciccia.aoc.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [taoensso.timbre :as timbre]))

(defn consume-starting-input
  [input]
  (reduce
    (fn [acc y]
      (apply merge
             acc
             (reduce
               (fn [acc2 x]
                 (assoc acc2 [x y 0] (get (get input y) x)))
               {}
               (range (count (first input))))))
    {}
    (range (count input))))

(defn gen-neighbors
  [[x y z]]
  (-> (combo/cartesian-product (range (dec x) (+ x 2))
                               (range (dec y) (+ y 2))
                               (range (dec z) (+ z 2)))
      (set)
      (disj [x y z])))

(defn transition
  [input loc]
  (let [active? (get input loc false)
        active-neighbors-count (->> (gen-neighbors loc)
                                    (filter #(get input % false))
                                    (count))]
    (if active?
      (<= 2 active-neighbors-count 3)
      (= 3 active-neighbors-count))))

(defn transition-all
  [input]
  (->> (keys input)
       (map gen-neighbors)
       (apply set/union)
       (map (fn [loc]
              (let [loc (vec loc)]
                [loc (transition input loc)])))
       (into {})))

(defn part1
  [input]
  (->> (iterate transition-all input)
       (drop 6)
       first
       (filter (fn [[_k v]] v))
       (count)))

(defn consume-starting-input2
  [input]
  (reduce
    (fn [acc y]
      (apply merge
             acc
             (reduce
               (fn [acc2 x]
                 (assoc acc2 [x y 0 0] (get (get input y) x)))
               {}
               (range (count (first input))))))
    {}
    (range (count input))))

(defn gen-neighbors2
  [[x y z w]]
  (-> (combo/cartesian-product (range (dec x) (+ x 2))
                               (range (dec y) (+ y 2))
                               (range (dec z) (+ z 2))
                               (range (dec w) (+ w 2)))
      (set)
      (disj [x y z w])))

(defn transition2
  [input loc]
  (let [active? (get input loc false)
        active-neighbors-count (->> (gen-neighbors2 loc)
                                    (filter #(get input % false))
                                    (count))]
    (if active?
      (<= 2 active-neighbors-count 3)
      (= 3 active-neighbors-count))))

(defn transition-all2
  [input]
  (->> (keys input)
       (map gen-neighbors2)
       (apply set/union)
       (map (fn [loc]
              (let [loc (vec loc)]
                [loc (transition2 input loc)])))
       (into {})))

(defn part2
  [input]
  (->> (iterate transition-all2 input)
       (drop 6)
       first
       (filter (fn [[_k v]] v))
       (count)))

(comment
  (consume-starting-input (utils/load-edn-input "day17.edn")))

(comment
  (part1 (consume-starting-input (utils/load-edn-input "day17-sample.edn"))))

(comment
  (part1 (consume-starting-input (utils/load-edn-input "day17.edn"))))

(comment
  (part2 (consume-starting-input2 (utils/load-edn-input "day17.edn"))))
