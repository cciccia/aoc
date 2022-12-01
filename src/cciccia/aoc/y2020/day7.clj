(ns cciccia.aoc.y2020.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input
  []
  (with-open [rdr (io/reader (io/resource "day7.txt"))]
    (reduce (fn [acc line]
              (let [[_ container contained-str] (re-matches #"(.*) bags contain (.*)." line)]
                (merge-with set/union acc (let [contained (mapv (fn [contained-item]
                                                                  (rest (re-matches #"(\d+) (.*) bags?" contained-item)))
                                                                (str/split contained-str #", "))]
                                            (when (seq (first contained))
                                              (into {} (map #(vector (last %) #{container}) contained)))))))
            {}
            (line-seq rdr))))

(defn all-containers
  [graph key]
  (apply conj #{key} (mapcat (partial all-containers graph) (graph key))))

(defn part1
  [parsed-input]
  (count (disj (all-containers parsed-input "shiny gold") "shiny gold")))



(defn parse-input2
  []
  (with-open [rdr (io/reader (io/resource "day7.txt"))]
    (reduce (fn [acc line]
              (let [[_ container contained-str] (re-matches #"(.*) bags contain (.*)." line)]
                (merge acc (let [contained (mapv (fn [contained-item]
                                                   (vec (rest (re-matches #"(\d+) (.*) bags?" contained-item))))
                                                 (str/split contained-str #", "))]
                             (when (seq (first contained))
                               {container (map #(update % 0 read-string) contained)})))))
            {}
            (line-seq rdr))))

(defn sum-contained
  [graph key]
  (apply + (mapcat (fn [[n k]]
                     [n (* n (sum-contained graph k))]) (get graph key))))

(defn part2
  [parsed-input]
  (sum-contained parsed-input "shiny gold"))

(comment
  (part1 (parse-input)))

(comment
  (parse-input2))

(comment
  (part2 (parse-input2)))

