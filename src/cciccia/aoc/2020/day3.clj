(ns cciccia.aoc.2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input
  []
  (with-open [rdr (io/reader (io/resource "day3.txt"))]
    (mapv #(str/split % #"") (line-seq rdr))))

(defn loc
  [parsed-input x y]
  (when-let [row (get parsed-input y)]
    (get row (mod x (count row)))))

(defn part
  [parsed-input [sx sy]]
  (loop [trees 0
         x 0
         y 0]
    (let [thing (loc parsed-input x y)]
      (case thing
        "#"
        (recur (inc trees) (+ sx x) (+ sy y))

        "."
        (recur trees (+ sx x) (+ sy y))

        nil
        trees))))

(comment
  (part (parse-input) [3 1]))

(comment
  (* (part (parse-input) [1 1])
     (part (parse-input) [3 1])
     (part (parse-input) [5 1])
     (part (parse-input) [7 1])
     (part (parse-input) [1 2])))