(ns cciccia.aoc.y2023.day3
  (:require [cciccia.aoc.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn near-part?
  [grid points]
  (some (fn [point]
          (not (contains? #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "."}
                          (get grid point "."))))
        points))


(defn expand-around-number
  [x-min x-max y]
  (conj
    (mapcat
      (fn [i]
        [[i (dec y)] [i (inc y)]])
      (range (dec x-min) (+ x-max 2)))
    [(dec x-min) y] [(inc x-max) y]))

(defn part1
  [grid]
  (let [max-x (apply max (map first (keys grid)))
        max-y (apply max (map second (keys grid)))]
    (loop [x 0
           y 0
           building ""
           total 0]
      (cond
        (> y max-y)
        total

        (> x max-x)
        (recur 0 (inc y) ""
               (cond-> total
                       (and (seq building) (near-part? grid (expand-around-number (- x (count building)) (dec x) y)))
                       (+ (Integer/parseInt building))))

        (contains? #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (get grid [x y] "."))
        (recur (inc x) y (str building (get grid [x y] "."))
               total)

        :else
        (recur (inc x) y ""
               (cond-> total
                       (and (seq building) (near-part? grid (expand-around-number (- x (count building)) (dec x) y)))
                       (+ (Integer/parseInt building))))))))

(defn part2
  [grid]
  (let [max-x (apply max (map first (keys grid)))
        max-y (apply max (map second (keys grid)))
        [parts stars] (loop [x 0
                             y 0
                             building []
                             total []
                             stars []]
                        (cond
                          (> y max-y)
                          [total stars]

                          (> x max-x)
                          (recur 0 (inc y) [] (cond-> total (seq building) (conj building)) stars)

                          (contains? #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} (get grid [x y] "."))
                          (recur (inc x) y (conj building [x y]) total stars)

                          (= "*" (get grid [x y] "."))
                          (recur (inc x) y [] (cond-> total (seq building) (conj building)) (conj stars [x y]))

                          :else
                          (recur (inc x) y [] (cond-> total (seq building) (conj building)) stars)))]
    (->> stars
         (keep (fn [[x y]]
                 (let [matching (->> parts
                                     (filter (fn [part]
                                               (seq (set/intersection (set part) (set (expand-around-number x x y)))))))]
                   (when (= 2 (count matching))
                     (apply * (map (fn [matcher]
                                     (Integer/parseInt (str/join (map #(get grid %) matcher))))
                                   matching))))))
         (apply +))))

(comment
  (part1 (utils/two-d-grid->map (utils/lined-spaced-input->2d-vec-str "2023/day3.txt" #"")))
  (part2 (utils/two-d-grid->map (utils/lined-spaced-input->2d-vec-str "2023/day3.txt" #""))))
