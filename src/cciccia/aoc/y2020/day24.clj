(ns cciccia.aoc.y2020.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cciccia.aoc.utils :as utils]))

(defn consume
  [{:keys [line pos]}]
  (let [[x y] pos]
    (cond
      (str/starts-with? line "ne")
      {:line (str/join "" (drop 2 line))
       :pos  [(inc x) y]}
      (str/starts-with? line "se")
      {:line (str/join "" (drop 2 line))
       :pos  [x (inc y)]}
      (str/starts-with? line "nw")
      {:line (str/join "" (drop 2 line))
       :pos  [x (dec y)]}
      (str/starts-with? line "sw")
      {:line (str/join "" (drop 2 line))
       :pos  [(dec x) y]}
      (str/starts-with? line "e")
      {:line (str/join "" (drop 1 line))
       :pos  [(inc x) (inc y)]}
      (str/starts-with? line "w")
      {:line (str/join "" (drop 1 line))
       :pos  [(dec x) (dec y)]})))

(def consume-until (utils/do-until consume
                                   (fn [{:keys [line]}]
                                     (= "" line))))

(defn part1
  [file]
  (with-open [rdr (io/reader (io/resource file))]
    (->> (reduce
           (fn [acc line]
             (let [{:keys [pos]} (consume-until {:pos  [0 0]
                                                 :line line})]
               (if (contains? acc pos)
                 (disj acc pos)
                 (conj acc pos))))
           #{}
           (line-seq rdr)))))

(defn all-neighbors
  [[x y]]
  [[(inc x) y]
   [x (inc y)]
   [x (dec y)]
   [(dec x) y]
   [(inc x) (inc y)]
   [(dec x) (dec y)]])

(defn expand-grid
  [grid]
  (set (mapcat #(conj (all-neighbors %) %) grid)))

(defn transition
  [grid [x y]]
  (let [num-neighbors (->> (all-neighbors [x y])
                           (filter #(contains? grid %))
                           (count))]
    (if (contains? grid [x y])
      (<= 1 num-neighbors 2)
      (= num-neighbors 2))))

(defn transition-all
  [grid]
  (->> (expand-grid grid)
       (filter (partial transition grid))
       set))

(defn part2
  [grid moves]
  (first (drop moves (iterate transition-all grid))))

(comment
  (count (part2 (part1 "day24.txt") 100)))

(comment
  (count (part2 (part1 "day24-sample.txt") 1)))
