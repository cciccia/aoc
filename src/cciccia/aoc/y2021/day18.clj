(ns cciccia.aoc.y2021.day18
  (:require [clojure.zip :as zip]
            [clojure.math.combinatorics :as comb]
            [cciccia.aoc.utils :as utils]))

(defn combine-fish
  [f1 f2]
  [f1 f2])

(defn explode
  [orig-loc]
  (let [l (zip/node orig-loc)
        r (zip/node (zip/right orig-loc))]
    (loop [loc (zip/prev (zip/replace (zip/up orig-loc) 0))
           state :seek-l]
      (cond
        (and (= state :seek-l) (nil? (zip/prev loc)))
        (recur loc :pass-c)

        (zip/end? loc)
        loc

        (and (= state :seek-l) (not (zip/branch? loc)))
        (recur (zip/next (zip/edit loc + l)) :pass-c)

        (and (= state :pass-c) (not (zip/branch? loc)))
        (recur (zip/next loc) :seek-r)

        (and (= state :seek-r) (not (zip/branch? loc)))
        (zip/edit loc + r)

        (= state :seek-l)
        (recur (zip/prev loc) state)

        :else
        (recur (zip/next loc) state)))))

(defn split
  [loc]
  (-> loc
      (zip/edit #(vector (long (Math/floor (/ % 2))) (long (Math/ceil (/ % 2)))))))

(defn explode-if-possible
  [fish]
  (loop [loc (zip/vector-zip fish)]
    (cond
      (zip/end? loc)
      nil

      (and loc
           (not (zip/branch? loc))
           (zip/right loc)
           (not (zip/branch? (zip/right loc)))
           (> (count (zip/path loc)) 4))
      (zip/root (explode loc))

      :else
      (recur (zip/next loc)))))

(defn split-if-possible
  [fish]
  (loop [loc (zip/vector-zip fish)]
    (cond
      (zip/end? loc)
      nil

      (and (not (zip/branch? loc))
           (> (zip/node loc) 9))
      (zip/root (split loc))

      :else
      (recur (zip/next loc)))))

(defn reduce-fish
  [root]
  (reduce
    (fn [p _c]
      (or (explode-if-possible p)
          (split-if-possible p)
          (reduced p)))
    root
    (repeat "blah")))

(defn sum-fish
  [fish]
  (if (vector? fish)
    (+ (* 3 (sum-fish (first fish)))
       (* 2 (sum-fish (second fish))))
    fish))

(defn part1
  [inputs]
  (->> (reduce
         (fn [f1 f2]
           (println f1)
           (->> (combine-fish f1 f2)
                reduce-fish))
         (first inputs)
         (rest inputs))
       (sum-fish)))

(defn part2
  [inputs]
  (->> (comb/combinations inputs 2)
       (mapcat (juxt reverse identity))
       (pmap (fn [[f1 f2]]
               (->> (combine-fish f1 f2)
                    reduce-fish
                    sum-fish)))
       (apply max)))

(comment
  (part1 (utils/load-edn-input "2021/day18.edn"))
  (part2 (utils/load-edn-input "2021/day18.edn"))
  (part1 (utils/load-edn-input "2021/day18-sample.edn")))
