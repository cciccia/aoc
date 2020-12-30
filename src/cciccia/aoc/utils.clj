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