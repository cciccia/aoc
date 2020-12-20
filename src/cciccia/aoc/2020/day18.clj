(ns cciccia.aoc.2020.day18
  (:require [taoensso.timbre :as timbre]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn infix
  [args]
  (let [[l op r & rest] args
        left  (if (list? l)
                (infix l)
                l)
        right (if (list? r)
                (infix r)
                r)]
    (cond
      (nil? op)
      left

      (nil? rest)
      ((resolve op) left right)

      :else
      (infix (conj rest ((resolve op) left right))))))

(defn part1
  []
  (with-open [rdr (io/reader (io/resource "day18.txt"))]
    (->> (line-seq rdr)
         (map #(infix (edn/read-string (str "(" % ")"))))
         (apply +))))

(defn infix-add-first
  [args]
  (let [args (vec args)]
    (if-let [[op n] (or (some (fn [[i v]]
                                (when (= v '*)
                                  [* i]))
                              (map-indexed #(vector %1 %2) args))
                        (some (fn [[i v]]
                                (when (= v '+)
                                  [+ i]))
                              (map-indexed #(vector %1 %2) args)))]
      (op
       (infix-add-first (subvec args 0 n))
       (infix-add-first (subvec args (inc n))))
      (let [base (first args)]
        (if (coll? base)
          (infix-add-first base)
          base)))))

(defn part2
  []
  (with-open [rdr (io/reader (io/resource "day18.txt"))]
    (->> (line-seq rdr)
         (map #(infix-add-first (edn/read-string (str "(" % ")"))))
         (apply +))))


(comment
  (part1))

(comment
  (part2))
