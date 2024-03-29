(ns cciccia.aoc.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io PushbackReader)
           (clojure.lang PersistentQueue)))

(defn load-edn-input
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

(defn do-until
  [iterator terminator]
  (fn [x]
    (some
      (fn [y]
        (when (terminator y)
          y))
      (iterate iterator x))))

(defn transpose-2d-vector
  [v]
  (apply mapv vector v))

(defn update-if-exists
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

(defn converge
  [f x]
  (->> (iterate f x)
       (partition 2)
       (some (fn [[prev cur]]
               (when (= prev cur)
                 cur)))))

(comment
  (converge (fn [x]
              (min (inc x) 10))
            0))

(defn two-d-grid->map
  [input]
  (reduce
    (fn [p1 [y line]]
      (merge p1
             (reduce
               (fn [p2 [x n]]
                 (assoc p2 [x y] n))
               {}
               (map-indexed #(vector %1 %2) line))))
    {}
    (map-indexed #(vector %1 %2) input)))

(defn lined-spaced-input->str
  [resource-file]
  (with-open [rdr (io/reader (io/resource resource-file))]
    (mapv identity (line-seq rdr))))

(defn lined-spaced-input->2d-vec-str
  ([resource-file]
   (lined-spaced-input->2d-vec-str resource-file #" "))
  ([resource-file re]
   (with-open [rdr (io/reader (io/resource resource-file))]
     (->> (line-seq rdr)
          (mapv #(str/split % re))))))

(defn lined-spaced-input->2d-vec-int
  ([resource-file]
   (lined-spaced-input->2d-vec-int resource-file #" "))
  ([resource-file re]
   (with-open [rdr (io/reader (io/resource resource-file))]
     (->> (line-seq rdr)
          (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line re))))))))

(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))

(defn manhattan
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))
