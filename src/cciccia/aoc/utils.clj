(ns cciccia.aoc.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)))

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

(comment
  (let [max (->> (map (fn [[[x y]]])
                      (if (= axis-type "x")
                        x
                        y))
               sort
               reverse first)]))