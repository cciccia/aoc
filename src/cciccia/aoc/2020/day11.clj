(ns cciccia.aoc.2020.day11
  (:require [cciccia.aoc.utils :as utils]))

(defn get-at
  [state [x y]]
  (get state [x y] ""))

(defn occupied?
  [state [x y]]
  (= "#" (get-at state [x y])))

(defn num-surrounded-by
  [state [x y]]
  (->> [[(dec x) (dec y)]
        [(dec x) y]
        [(dec x) (inc y)]
        [x (dec y)]
        [x (inc y)]
        [(inc x) (dec y)]
        [(inc x) y]
        [(inc x) (inc y)]]
       (filter (partial occupied? state))
       (count)))

(defn transition
  [state [x y]]
  (case (get state [x y])
    "."
    "."

    "L"
    (if (zero? (num-surrounded-by state [x y]))
      "#"
      "L")

    "#"
    (if (>= (num-surrounded-by state [x y]) 4)
      "L"
      "#")))

(defn transition-all
  [state]
  (->> (keys state)
       (pmap (fn [[x y]]
               [[x y] (transition state [x y])]))
       (into {})))

(defn part1
  [state]
  (reduce
    (fn [before after]
      (if (= before after)
        (reduced (->> (vals before)
                      (filter (fn [v] (= "#" v)))
                      (count)))
        after))
    state
    (rest (iterate transition-all state))))

(defn pivot-input
  [input]
  (->> (for [y (range (count input))
             x (range (count (first input)))]
         [[x y] (get (get input y []) x ".")])
       (into {})))

(defn view-in-direction
  [state [x y] [dx dy]]
  (loop [px (+ x dx)
         py (+ y dy)]
    (let [test (get-at state [px py])]
      (if (= "." test)
        (recur (+ px dx) (+ py dy))
        test))))

(defn num-surrounded-by2
  [state [x y]]
  (->> [[-1 -1]
        [-1 0]
        [-1 1]
        [0 -1]
        [0 1]
        [1 -1]
        [1 0]
        [1 1]]
       (pmap (partial view-in-direction state [x y]))
       (filter #(= "#" %))
       (count)))

(defn transition2
  [state [x y]]
  (case (get state [x y])
    "."
    "."

    "L"
    (if (zero? (num-surrounded-by2 state [x y]))
      "#"
      "L")

    "#"
    (if (>= (num-surrounded-by2 state [x y]) 5)
      "L"
      "#")))

(defn transition-all2
  [state]
  (->> (keys state)
       (pmap (fn [[x y]]
               [[x y] (transition2 state [x y])]))
       (into {})))

(defn part2
  [state]
  (reduce
    (fn [before after]
      (if (= before after)
        (reduced (->> (vals before)
                      (filter (fn [v] (= "#" v)))
                      (count)))
        after))
    state
    (rest (iterate transition-all2 state))))

(comment
  (part1 (pivot-input (utils/load-edn-input "day11.edn"))))

(comment
  (part2 (pivot-input (utils/load-edn-input "day11.edn"))))

