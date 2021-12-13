(ns cciccia.aoc.2021.day13
  (:require [cciccia.aoc.utils :as utils]))

(defn make-grid
  [input]
  (reduce
    (fn [p [x y]]
      (conj p [x y]))
    #{}
    input))

(defn fold
  [grid [axis-type axis-num]]
  (reduce
    (fn [p [x y]]
      (case axis-type
        "x"
        (conj p [(- axis-num (Math/abs ^long (- axis-num x))) y])

        "y"
        (conj p [x (- axis-num (Math/abs ^long (- axis-num y)))])))
    #{}
    grid))

(defn part1
  [grid fold-instructions]
  (reduce
    (fn [p fold-instruction]
      (fold p fold-instruction))
    grid
    fold-instructions))

(defn print-it
  [grid]
  (let [max-x (->> (map (fn [[x _y]] x) grid)
                   sort
                   reverse
                   first
                   inc)
        max-y (->> (map (fn [[_x y]] y) grid)
                   sort
                   reverse
                   first
                   inc)]
    (doseq [y (range max-y)]
      (doseq [x (range max-x)]
        (print (if (contains? grid [x y]) "#" " ")))
      (print "\n"))))

(comment
  (count (part1 (make-grid (utils/load-edn-input "2021/day13-dots.edn"))
                (take 1 (utils/load-edn-input "2021/day13-folds.edn"))))
  (-> (part1 (make-grid (utils/load-edn-input "2021/day13-dots.edn"))
             (utils/load-edn-input "2021/day13-folds.edn"))
      print-it))