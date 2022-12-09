(ns cciccia.aoc.y2022.day9
  (:require [cciccia.aoc.utils :as utils]))

(defn- should-move?
  [[hx hy] [tx ty]]
  (or (> (Math/abs ^Integer (- hx tx)) 1)
      (> (Math/abs ^Integer (- hy ty)) 1)))

(defn- new-tail
  [head tail]
  (if-not (should-move? head tail)
    tail
    (let [[hx hy] head
          [tx ty] tail
          tx2 (cond
                (< tx hx)
                (inc tx)
                (> tx hx)
                (dec tx)
                :else tx)
          ty2 (cond
                (< ty hy)
                (inc ty)
                (> ty hy)
                (dec ty)
                :else ty)]
      [tx2 ty2])))

(defn- process-events
  [num-actors system]
  (reduce
    (fn [{:keys [actors] :as acc} i]
      (let [[hx hy] (get actors (dec i))
            [tx ty] (new-tail [hx hy] (get actors i))]
        (-> acc
            (assoc-in [:actors i] [tx ty])
            (update-in [:v-actors i] conj [tx ty]))))
    system
    (range 1 num-actors)))

(defn part1
  [input num-actors]
  (-> (reduce
        (fn [acc [dir times]]
          (last (take (inc times) (iterate
                                    (fn [system]
                                      (process-events num-actors
                                                      (-> system
                                                          (update-in [:actors 0] (fn [[x y]]
                                                                                   (case dir
                                                                                     "U" [x (inc y)]
                                                                                     "D" [x (dec y)]
                                                                                     "L" [(dec x) y]
                                                                                     "R" [(inc x) y]))))))
                                    acc))))
        (reduce
          (fn [acc i]
            (-> acc
                (update :actors assoc i [0 0])
                (update :v-actors assoc i #{[0 0]})))
          {:actors {} :v-actors {}}
          (range num-actors))
        input)
      :v-actors
      (get (dec num-actors))
      count))
(comment
  (part1 (utils/load-edn-input "2022/day9.edn") 10))
